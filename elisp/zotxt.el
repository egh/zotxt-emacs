;;; zotxt.el --- Interface emacs with Zotero via the zotxt extension
     
;; Copyright (C) 2010-2014 Erik Hetzner

;; Author: Erik Hetzner <egh@e6h.org>
;; Keywords: bib

;; This file is not part of GNU Emacs.

;; zotxt.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; zotxt.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with zotxt.el. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'json)
(require 'request)
(require 'deferred)
(require 'request-deferred)

(defvar zotxt-default-bibliography-style
  "chicago-note-bibliography"
  "Default bibliography style to use.")

(defvar zotxt-url-base
  "http://127.0.0.1:23119/zotxt"
  "Base URL to contact.")

(defvar zotxt-url-items
  (format "%s/items" zotxt-url-base)
  "Items URL to contact.")

(defun zotxt-mapcar-deferred (func lst)
  (apply #'deferred:parallel
         (mapcar func lst)))

(defun zotxt-clean-bib-entry (entry)
  "Clean up a bibliography ENTRY as returned by Zotxt."
  (let ((retval entry))
    (setq retval (replace-regexp-in-string "\n" "" retval))
    (setq retval (replace-regexp-in-string "\" "“" retval))
    (setq retval (replace-regexp-in-string "\" "’" retval))
    (setq retval (replace-regexp-in-string "\^]" "”" retval))
    retval))

(defun zotxt-get-item-bibliography-deferred (item)
  "Retrieve the generated bibliography for ITEM (a plist).
Use STYLE to specify a custom bibliography style.
Adds a plist entry with the name of the style as a self-quoting symbol, e.g.
:chicago-note-bibliography.
Also adds :citation entry if STYLE is the default."
  (lexical-let ((d (deferred:new))
                (style zotxt-default-bibliography-style)
                (item item))
    (request
     zotxt-url-items
     :params `(("key" . ,(plist-get item :key))
               ("format" . "bibliography")
               ("style" . ,style))
     :parser 'json-read
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (let* ((style-key (intern (format ":%s" style)))
                        (first (elt data 0))
                        (text (zotxt-clean-bib-entry (cdr (assq 'text first)))))
                   (if (string= style zotxt-default-bibliography-style)
                       (plist-put item :citation text))
                   (plist-put item style-key text)
                   (deferred:callback-post d item)))))
    d))

(defun zotxt-get-selected-items-deferred ()
  (lexical-let ((d (deferred:new)))
    (request
     zotxt-url-items
     :params '(("selected" . "selected")
               ("format" . "key"))
     :parser 'json-read
     :success (function*
               (lambda (&key data &allow-other-keys)
                     (deferred:callback-post
                       d (mapcar (lambda (k)
                                   (list :key k))
                                 data)))))
      d))

(defun zotxt-choose-deferred ()
  "Prompt a user for a search string, then ask the user to select an item from the citation."
  (let* ((search-string
          (read-from-minibuffer "Zotero quicksearch query: ")))
    (lexical-let ((d (deferred:new)))
      (request
       "http://127.0.0.1:23119/zotxt/search"
       :params `(("q" . ,search-string)
                 ("format" . "bibliography"))
       :parser 'json-read
       :success (function*
                 (lambda (&key data &allow-other-keys)
                   (let* ((results (mapcar (lambda (e) 
                                             (cons (cdr (assq 'text e)) 
                                                   (cdr (assq 'key e))))
                                           data))
                          (count (length results))
                          (citation (if (= 0 count)
                                        nil
                                      (if (= 1 count)
                                          (car (car results))
                                        (completing-read "Select item: " results))))
                          (key (cdr (assoc-string citation results))))
                     (deferred:callback-post
                       d `((:key ,key :citation ,citation)))))))
      d)))

(defun zotxt-select-easykey (easykey)
  (request
   "http://127.0.0.1:23119/zotxt/select"
   :params `(("easykey" . ,easykey))))

(defun zotxt-select-key (key)
  (request
   "http://127.0.0.1:23119/zotxt/select"
   :params `(("key" . ,key))))

(defvar zotxt-easykey-regex
  "[@{]\\([[:alnum:]:]+\\)")

(defvar zotxt-easykey-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c \" o") 'zotxt-easykey-select-item-at-point)
    (define-key map (kbd "C-c \" k") 'zotxt-easykey-insert)
    map))

(defun zotxt-easykey-at-point-match ()
  (or (looking-at zotxt-easykey-regex)
      (save-excursion
        ;; always try to back up one char
        (backward-char)
        (while (and (not (looking-at zotxt-easykey-regex))
                    (looking-at "[[:alnum:]:]"))
          (backward-char))
        (looking-at zotxt-easykey-regex))))

(defun zotxt-easykey-at-point ()
  "Return the value of the easykey at point. Easykey must start
with a @ or { to be recognized, but this will *not* be returned."
  (save-excursion
    (if (zotxt-easykey-at-point-match)
        (match-string 1)
      nil)))
  
(defun zotxt-easykey-complete-at-point ()
  (save-excursion
    (if (not (zotxt-easykey-at-point-match))
        nil
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (key (match-string 1))
             (completions
              (deferred:$
                (request-deferred
                 "http://127.0.0.1:23119/zotxt/complete"
                 :params `(("easykey" . ,key))
                 :parser 'json-read)
                (deferred:nextc it
                  (lambda (response)
                    (mapcar (lambda (k) (format "@%s" k))
                            (request-response-data response))))
                (deferred:sync! it))))
        (if (null completions)
            nil
          (list start end completions))))))

(defun zotxt-get-item-easykey (item)
  "Given a plist ITEM, add the :easykey corresponding to the :key value.
Non-deferred version of `zotxt-get-item-easykey-deferred'."
  (deferred:$
    (zotxt-get-item-easykey-deferred item)
    (deferred:sync! it)))
    
(defun zotxt-get-item-easykey-deferred (item)
  "Given a plist ITEM, add the :easykey corresponding to the :key value."
  (lexical-let ((item item)
                (d (deferred:new)))
    (request
     zotxt-url-items
     :params `(("key" . ,(plist-get item :key))
               ("format" . "easykey"))
     :parser 'json-read
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (plist-put item :easykey (elt data 0))
                 (deferred:callback-post d item))))
    d))

(defun zotxt-easykey-insert (arg)
  "Prompt for a search string and insert an easy key. With C-u,
insert easykeys for the currently selected items in Zotero."
  (interactive "P")
  (lexical-let ((mk (point-marker)))
    (deferred:$
      (if arg
          (zotxt-get-selected-items-deferred)
        (zotxt-choose-deferred))
      (deferred:nextc it
        (lambda (items)
          (zotxt-mapcar-deferred #'zotxt-get-item-easykey-deferred items)))
      (deferred:nextc it
        (lambda (items)
          (with-current-buffer (marker-buffer mk)
            (goto-char (marker-position mk))
            (insert (mapconcat
                     (lambda (item)
                       (format "@%s" (plist-get item :easykey)))
                     items " "))))))))

(defun zotxt-easykey-select-item-at-point ()
  "Select the item referred to by the easykey at point in Zotero."
  (interactive)
  (zotxt-select-easykey (zotxt-easykey-at-point)))

;;;###autoload
(define-minor-mode zotxt-easykey-mode
  "Toggle zotxt-easykey-mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

This is a minor mode for managing your easykey citations,
including completion."
  :init-value nil
  :lighter " ZotEasykey"
  :keymap zotxt-easykey-mode-map
  (if zotxt-easykey-mode
      (setq-local completion-at-point-functions
                  (cons 'zotxt-easykey-complete-at-point
                        completion-at-point-functions))
    (setq-local completion-at-point-functions
                (remove 'zotxt-easykey-complete-at-point 
                        completion-at-point-functions))))

(provide 'zotxt)
;;; zotxt.el ends here
