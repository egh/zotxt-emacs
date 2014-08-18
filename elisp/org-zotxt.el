;;; org-zotxt.el --- Interface org-mode with Zotero via the zotxt extension
     
;; Copyright (C) 2010-2014 Erik Hetzner

;; Author: Erik Hetzner <egh@e6h.org>
;; Keywords: bib

;; This file is not part of GNU Emacs.

;; org-zotxt.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; org-zotxt.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with org-zotxt.el. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'request)
(require 'org)
(require 'zotxt)

(defun org-zotxt-extract-link-id-from-link (path)
  "Return the zotxt ID from a link PATH."
  (if (string-match "^zotero://select/items/\\(.*\\)$" path)
      (match-string 1 path)
    nil))

(defun org-zotxt-update-reference-link-at-point ()
  "Update the zotero:// link at point."
  (interactive)
  (let* ((ct (org-element-context))
         (link (org-element-property :raw-link ct)))
    (if (eq 'link (org-element-type ct))
        (lexical-let ((mk (point-marker))
                      (item-id (org-zotxt-extract-link-id-from-link link)))
          (deferred:$
            (deferred:next (lambda () `(:key ,item-id)))
            (deferred:nextc it
              (lambda (item)
                (zotxt-get-item-bibliography-deferred item)))
            (deferred:nextc it
              (lambda (item)
                (save-excursion
                  (with-current-buffer (marker-buffer mk)
                    (goto-char (marker-position mk))
                    (let ((ct (org-element-context)))
                      (goto-char (org-element-property :begin ct))
                      (delete-region (org-element-property :begin ct)
                                     (org-element-property :end ct))
                      (insert (format "[[zotero://select/items/%s][%s]]"
                                      (plist-get item :key)
                                      (plist-get item :bibliography)))))))))))))

(defun org-zotxt-update-all-reference-links ()
  "Update all zotero:// links in a document."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((next-link (org-element-link-successor)))
      (while (not (null next-link))
        (goto-char (cdr next-link))
        (let* ((parse (org-element-link-parser))
               (path (org-element-property :raw-link parse))
               (end (org-element-property :end parse)))
          (if (org-zotxt-extract-link-id-from-link path)
              (org-zotxt-update-reference-link-at-point))
          (goto-char end))
        (setq next-link (org-element-link-successor))))))

(defun org-zotxt-get-reference-link ()
  "Prompts for item and returns a reference link suitable for
insertion in org-mode. Useful for capture templates."
  (let ((item (zotxt-choose)))
    (format
     "[[zotero://select/items/%s][%s]]\n" (cdr item) (car item))))

(defun org-zotxt-insert-reference-link (arg)
  "Insert a zotero link in the org-mode document. Prompts for
search to choose item. If prefix argument (C-u) is used, will
insert the currently selected item from Zotero."
  (interactive "P")
  (if arg 
      (let ((ids (zotxt-get-selected-item-ids)))
        (mapc (lambda (id)
                (insert (format
                         "[[zotero://select/items/%s][%s]]\n"
                         id id))
                (org-zotxt-update-reference-link-at-point)
                (forward-line 1))
              ids))
    (let ((item (zotxt-choose)))
      (insert (format
               "[[zotero://select/items/%s][%s]]\n" (cdr item) (car item))))))

(org-add-link-type "zotero"
                   (lambda (rest)
                     (zotxt-select-key (substring rest 15))))

(defvar org-zotxt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c \" i") 'org-zotxt-insert-reference-link)
    (define-key map (kbd "C-c \" u") 'org-zotxt-update-reference-link-at-point)
    map))

(defun org-zotxt-choose-path (paths)
  "Prompt user to select a path from the PATHS.
If only path is available, return it.  If no paths are available, error."
  (if (= 0 (length paths))
      (progn (message "No attachments for item!")
             (error "No attachments for item!"))
    (if (= 1 (length paths))
        (elt paths 0)
      (completing-read "File: " (append paths nil)))))

(defun org-zotxt-open-attachment (arg)
  "Open a Zotero items attachment.
Prefix ARG means open in Emacs."
  (interactive "P")
  (lexical-let ((arg arg))
    (zotxt-choose-async
     (lambda (items)
       (let ((item (car items)))
         (request
          zotxt-url-items
          :params `(("key" . ,(plist-get item :key)) ("format" . "recoll"))
          :parser 'json-read
          :success (function*
                    (lambda (&key data &allow-other-keys)
                      (let ((paths (cdr (assq 'paths (elt data 0)))))
                        (org-open-file (org-zotxt-choose-path paths) arg))))))))))

;;;###autoload
(define-minor-mode org-zotxt-mode
  "Toggle org-zotxt-mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

This is a minor mode for managing your citations with Zotero in a
org-mode document."  
  nil
  " OrgZot"
  org-zotxt-mode-map)

(provide 'org-zotxt)
;;; org-zotxt.el ends here
