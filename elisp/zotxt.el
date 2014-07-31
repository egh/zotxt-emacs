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

(defun zotxt-generate-bib-entry-from-id (item-id &optional style bib-format)
  (let* ((url (format "http://127.0.0.1:23119/zotxt/items?key=%s&format=bibliography&style=%s"
                      (url-hexify-string item-id)
                      (url-hexify-string (or style zotxt-default-bibliography-style))))
         (results (zotxt-url-retrieve url))
         (first (elt results 0))
         (text (cdr (assq 'text first))))
    (zotxt-clean-bib-entry text)))

(defun zotxt-get-selected-item-ids ()
  (zotxt-url-retrieve "http://127.0.0.1:23119/zotxt/items?selected=selected&format=key"))

(defun zotxt-get-item-json (key format success)
  "Get info about an item with KEY in FORMAT; returns parsed JSON."
  (request
   zotxt-url-item
   :params '(("key" . key) ("format" . format))
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
               data))))

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

(defun zotxt-select-easykey (easykey)
  (let ((url (format "http://127.0.0.1:23119/zotxt/select?easykey=%s"
                    (url-hexify-string easykey))))
    (zotxt-url-retrieve url)))

(defun zotxt-select-key (key)
  (let ((url (format "http://127.0.0.1:23119/zotxt/select?key=%s"
                    (url-hexify-string key))))
    (zotxt-url-retrieve url)))

(provide 'zotxt)
;;; zotxt.el ends here
