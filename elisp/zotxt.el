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

(require 'url-http)
(require 'url-handlers)
(require 'json)
(require 'request)

(defvar zotxt-default-bibliography-style
  "http://www.zotero.org/styles/chicago-note-bibliography"
  "Default bibliography style to use.")

(defvar zotxt-url-base
  "http://127.0.0.1:23119/zotxt"
  "Base URL to contact.")

(defvar zotxt-url-item
  (format "%s/items" zotxt-url-base)
  "Items URL to contact.")

(defun zotxt-url-get-body-as-string ()
  (with-temp-buffer
    (url-insert (current-buffer))
    (buffer-string)))

(defun zotxt-url-retrieve (url)
  (save-excursion
    (let (url-http-end-of-headers ; prevent warnings about free variables
          url-http-response-status
          (buff (url-retrieve-synchronously url)))
      (set-buffer buff)
      (if (not url-http-end-of-headers)
          (error "Did not receive data from %s" url))
      (url-http-parse-response)
      (cond ((eq 400 url-http-response-status)
             (error "Client error from server with message: %s" 
                    (zotxt-url-get-body-as-string)))
            ((eq 500 url-http-response-status)
             (error "Server error from server with message: %s"
                    (zotxt-url-get-body-as-string)))
            ((eq 200 url-http-response-status)
             (with-temp-buffer
               (url-insert buff)
               (goto-char (point-min))
               (json-read)))
            (t
             (error "Unexpected response from server: %d" 
                    url-http-response-status))))))

(defun zotxt-clean-bib-entry (entry)
  "Clean up a bibliography entry as returned by Zotxt."
  (let ((retval entry))
    (setq retval (replace-regexp-in-string "\n" "" retval))
    (setq retval (replace-regexp-in-string "\" "“" retval))
    (setq retval (replace-regexp-in-string "\" "’" retval))
    (setq retval (replace-regexp-in-string "\^]" "”" retval))
    retval))

(cl-defun zotxt-generate-bib-entry-from-id (item-id &key
                                                    callback
                                                    (style zotxt-default-bibliography-style))
  "Retrieve the generated bibliography for ITEM-ID.
Call CALLBACK with text of bibliography entry.  Use STYLE to
specify a custom bibliography style."
  (lexical-let ((callback1 callback))
    (request
     "http://127.0.0.1:23119/zotxt/items"
     :params `(("key" . ,item-id)
               ("format" . "bibliography")
               ("style" . ,style))
     :parser 'json-read
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (let* ((first (elt data 0))
                        (text (cdr (assq 'text first))))
                   (funcall callback1 text)))))))

(defun zotxt-get-selected-item-ids ()
  (zotxt-url-retrieve "http://127.0.0.1:23119/zotxt/items?selected=selected&format=key"))

(defun zotxt-search (q format)
  (zotxt-url-retrieve (format "http://127.0.0.1:23119/zotxt/search?q=%s&format=%s" 
                               (url-hexify-string q)
                               format)))

(defun zotxt-choose ()
  "Prompt a user for a search string, then ask the user to select
an item from the citation. Returns (citation . key)."
  (let* ((search-string (read-from-minibuffer "Zotero quicksearch query: "))
         (results (mapcar (lambda (e) 
                            (cons (cdr (assq 'text e)) 
                                  (cdr (assq 'key e))))
                          (zotxt-search search-string "bibliography")))
         (count (length results))
         (item (if (= 0 count)
                   nil
                 (if (= 1 count)
                     (car (car results))
                   (completing-read "Select item: " results)))))
    (assoc-string item results)))

(defun zotxt-choose-async (func)
  "Prompt a user for a search string, then ask the user to select an item from the citation.
Calls FUNC with args (key citation)."
  (let* ((search-string (read-from-minibuffer "Zotero quicksearch query: ")))
    (lexical-let ((func1 func))
      (request
       "http://127.0.0.1:23119/zotxt/search"
       :params `(("q" . ,search-string)
                 (format . "bibliography"))
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
                     (funcall func1 key citation))))))))

(defun zotxt-select-easykey (easykey)
  (let ((url (format "http://127.0.0.1:23119/zotxt/select?easykey=%s"
                    (url-hexify-string easykey))))
    (zotxt-url-retrieve url)))

(defun zotxt-select-key (key)
  (let ((url (format "http://127.0.0.1:23119/zotxt/select?key=%s"
                    (url-hexify-string key))))
    (zotxt-url-retrieve url)))

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
      (let ((start (match-beginning 0))
            (end (match-end 0))
            (key (match-string 1)))
        (let* ((url (format "http://127.0.0.1:23119/zotxt/complete?easykey=%s" key))
               (response (zotxt-url-retrieve url)))
          (if (null response)
              nil
            (let ((completions (mapcar (lambda (k) (format "@%s" k)) response)))
              (list start end completions))))))))

(defun zotxt-easykey-get-item-id-at-point ()
  "Return the Zotero ID of the item referred to by the easykey at
point, or nil."
  (save-excursion
    (let ((key (zotxt-easykey-at-point)))
      (if (null key)
          nil
        (let* ((url (format "http://127.0.0.1:23119/zotxt/items?format=key&easykey=%s" key))
               (response (zotxt-url-retrieve url)))
          (if (null response)
              nil
            (elt response 0)))))))

(defun zotxt-easykey-get-item-easykey (key)
  (elt (zotxt-url-retrieve
        (format "http://127.0.0.1:23119/zotxt/items?key=%s&format=easykey" key)) 0))

(defun zotxt-easykey-insert (arg)
  "Prompt for a search string and insert an easy key. With C-u,
insert easykeys for the currently selected items in Zotero."
  (interactive "P")
  (let ((keys (if arg
                 (zotxt-get-selected-item-ids)
                (list (cdr (zotxt-choose))))))
    (insert (mapconcat (lambda (key)
                         (format "@%s" (zotxt-easykey-get-item-easykey key))) keys " "))))

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
