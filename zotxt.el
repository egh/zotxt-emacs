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

(defconst zotxt-url-base
  "http://127.0.0.1:23119/zotxt"
  "Base URL to contact.")

(defconst zotxt-url-search
  "Search URL to contact.")

(defvar zotxt--debug-sync nil
  "Use synchronous requests.  For debug only!")

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

(defun zotxt-completing-read (&rest args)
  "Use ido-completing-read if ido-mode is t, else use completing-read."
  (if ido-mode
      (apply #'ido-completing-read args)
    (apply #'completing-read args)))

(defun zotxt-get-item-bibliography-deferred (item)
  "Retrieve the generated bibliography for ITEM (a plist).
Use STYLE to specify a custom bibliography style.
Adds a plist entry with the name of the style as a self-quoting symbol, e.g.
:chicago-note-bibliography.
Also adds :citation entry if STYLE is the default.
Also adds HTML versions, suffixed with -html"
  (lexical-let ((d (deferred:new))
                (style zotxt-default-bibliography-style)
                (item item))
    (if (and (string= style zotxt-default-bibliography-style)
             (plist-get item :citation))
        ;; item already has citation, no need to fetch
        (deferred:callback-post d item)
      (request
       (format "%s/items" zotxt-url-base)
       :params `(("key" . ,(plist-get item :key))
                 ("format" . "bibliography")
                 ("style" . ,style))
       :parser 'json-read
       :success (function*
                 (lambda (&key data &allow-other-keys)
                   (let* ((style-key (intern (format ":%s" style)))
                          (style-key-html (intern (format ":%s-html" style)))
                          (first (elt data 0))
                          (text (zotxt-clean-bib-entry (cdr (assq 'text first))))
                          (html (cdr (assq 'html first))))
                     (if (string= style zotxt-default-bibliography-style)
                         (progn
                           (plist-put item :citation text)
                           (plist-put item :citation-html html)))
                     (plist-put item style-key text)
                     (plist-put item style-key-html html)
                     (deferred:callback-post d item))))))
    d))

(defun zotxt-get-selected-items-deferred ()
  (lexical-let ((d (deferred:new)))
    (request
     (format "%s/items" zotxt-url-base)
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

(defconst zotxt-quicksearch-method-names
  '(("title, creator, year" . :title-creator-year)
    ("fields" . :fields)
    ("everything" . :everything)))

(defconst  zotxt-quicksearch-method-params
  '((:title-creator-year . "titleCreatorYear")
    (:fields . "fields")
    (:everything . "everything")))

(defconst zotxt-quicksearch-method-to-names
  '((:title-creator-year . "title, creator, year")
    (:fields . "fields")
    (:everything . "everything")))

(defun zotxt-choose-deferred (&optional method search-string)
  "Prompt a user for a search string, then ask the user to select an item from the citation.

If METHOD is supplied, it should be one of :title-creator-year, :fields, or :everything.
If SEARCH-STRING is supplied, it should be the search string."
  (if (null method)
      (let ((method-name 
             (zotxt-completing-read
              "Zotero search method (nothing for title, creator, year): "
              zotxt-quicksearch-method-names
              nil t nil nil "title, creator, year")))
        (setq method (cdr (assoc method-name zotxt-quicksearch-method-names)))))
  (if (null search-string)
      (setq search-string
            (read-string (format "Zotero quicksearch (%s) query: " (cdr (assq method zotxt-quicksearch-method-to-names))))))
  (lexical-let ((d (deferred:new)))
    (request
     (format "%s/search" zotxt-url-base)
     :params `(("q" . ,search-string)
               ("method" . ,(cdr (assq method zotxt-quicksearch-method-params)))
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
                                      (zotxt-completing-read "Select item: " results))))
                        (key (cdr (assoc-string citation results))))
                   (deferred:callback-post
                     d (if (null citation) nil
                         `((:key ,key :citation ,citation))))))))
    d))

(defun zotxt-select-easykey (easykey)
  (request
   (format "%s/select" zotxt-url-base)
   :params `(("easykey" . ,easykey))))

(defun zotxt-select-key (key)
  (request
   (format "%s/select" zotxt-url-base)
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
                 (format "%s/complete" zotxt-url-base)
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

(defun zotxt-get-item-deferred (item format)
  "Given a plist ITEM, add the FORMAT."
  (lexical-let ((item item)
                (format format)
                (d (deferred:new)))
    (request
     (format "%s/items" zotxt-url-base)
     :params `(("key" . ,(plist-get item :key))
               ("format" . ,(substring (symbol-name format) 1)))
     :parser (if (or (eq format :easykey)
                     (eq format :betterbibtexkey))
                 #'json-read
               #'buffer-string)
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (if (or (eq format :easykey)
                         (eq format :betterbibtexkey))
                     ;; json data
                     (plist-put item format (elt data 0))
                   (plist-put item format data))
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
          (zotxt-mapcar-deferred (lambda (item)
                                   (zotxt-get-item-deferred item :easykey))
                                 items)))
      (deferred:nextc it
        (lambda (items)
          (with-current-buffer (marker-buffer mk)
            (goto-char (marker-position mk))
            (insert (mapconcat
                     (lambda (item)
                       (format "@%s" (plist-get item :easykey)))
                     items " ")))))
      (if zotxt--debug-sync (deferred:sync! it)))))

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
