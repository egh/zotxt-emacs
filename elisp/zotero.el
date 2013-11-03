(require 'url-util)
(require 'url-handlers)
(require 'json)

(defvar zotero-default-bibliography-style
  "http://www.zotero.org/styles/chicago-note-bibliography"
  "Default bibliography style to use.")

(defun zotero-url-get-body-as-string ()
  (with-temp-buffer
    (url-insert buff)
    (buffer-string)))

(defun zotero-url-retrieve (raw-url)
  (save-excursion
    (let* ((url (url-encode-url raw-url))
           (buff (url-retrieve-synchronously url)))
      (set-buffer buff)
      (url-http-parse-response)
      (cond ((eq 400 url-http-response-status)
             (error "Client error from server with message: %s" 
                    (zotero-url-get-body-as-string)))
            ((eq 500 url-http-response-status)
             (error "Server error from server with message: %s"
                    (zotero-url-get-body-as-string)))
            ((eq 200 url-http-response-status)
             (with-temp-buffer
               (url-insert buff)
               (beginning-of-buffer)
               (json-read)))
            (t
             (error "Unexpected response from server: %d" 
                    url-http-response-status))))))

(defun zotero-clean-bib-entry (entry)
  "Clean up a bibliography entry as returned by Zotero."
  (let ((retval entry))
    (setq retval (replace-regexp-in-string "\n" "" retval))
    (setq retval (replace-regexp-in-string "\" "“" retval))
    (setq retval (replace-regexp-in-string "\" "’" retval))
    (setq retval (replace-regexp-in-string "\^]" "”" retval))
    retval))

(defun zotero-generate-bib-entry-from-id (item-id &optional style bib-format)
  (let* ((url (format "http://localhost:23119/zotxt/items?key=%s&format=bibliography&style=%s"
                      item-id (or style zotero-default-bibliography-style)))
         (results (zotero-url-retrieve url))
         (first (elt results 0))
         (text (cdr (assq 'text first))))
    (zotero-clean-bib-entry text)))

(defun zotero-get-selected-item-ids ()
  (zotero-url-retrieve "http://localhost:23119/zotxt/items?selected=selected&format=key"))

(provide 'zotero)
