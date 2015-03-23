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
(require 'org-element)
(require 'zotxt)

(defcustom org-zotxt-link-description-style
  :citation
  "Style to use for org zotxt link texts."
  :group 'org-zotxt
  :type '(choice (const :tag "easykey" :easykey)
                 (const :tag "better BibTeX" :betterbibtexkey)
                 (const :tag "citation" :citation)))

(defun org-zotxt-extract-link-id-at-point ()
  "Extract the Zotero key of the link at point."
  (let ((ct (org-element-context)))
    (if (eq 'link (org-element-type ct))
        (org-zotxt-extract-link-id-from-path (org-element-property :path ct))
      nil)))

(defun org-zotxt-extract-link-id-from-path (path)
  "Return the zotxt ID from a link PATH."
  (if (string-match "^//select/items/\\(.*\\)$" path)
      (match-string 1 path)
    nil))

(defun org-zotxt-insert-reference-link-to-item (item)
  "Insert link to Zotero ITEM in buffer."
  (insert (org-make-link-string (format "zotero://select/items/%s"
                                        (plist-get item :key))
                                (if (or (eq org-zotxt-link-description-style :easykey)
                                        (eq org-zotxt-link-description-style :betterbibtexkey))
                                    (concat "@" (plist-get item org-zotxt-link-description-style))
                                  (plist-get item :citation)))))

(defun org-zotxt-insert-reference-links-to-items (items)
  "Insert links to Zotero ITEMS in buffer."
  (mapc (lambda (item)
          (org-zotxt-insert-reference-link-to-item item)
          (insert "\n")
          (forward-line 1))
        items))

(defun org-zotxt-update-reference-link-at-point ()
  "Update the zotero:// link at point."
  (interactive)
  (lexical-let ((mk (point-marker))
                (item-id (org-zotxt-extract-link-id-at-point)))
    (if item-id
        (deferred:$
          (deferred:next (lambda () `(:key ,item-id)))
          (deferred:nextc it
            (lambda (item)
              (org-zotxt-get-item-link-text-deferred item)))
          (deferred:nextc it
            (lambda (item)
              (save-excursion
                (with-current-buffer (marker-buffer mk)
                  (goto-char (marker-position mk))
                  (let ((ct (org-element-context)))
                    (goto-char (org-element-property :begin ct))
                    (delete-region (org-element-property :begin ct)
                                   (org-element-property :end ct))
                    (org-zotxt-insert-reference-link-to-item item))))))
          (if zotxt--debug-sync (deferred:sync! it))))))

(defun org-zotxt-update-all-reference-links ()
  "Update all zotero:// links in a document."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((next-link (org-element-link-successor)))
      (while (not (null next-link))
        (goto-char (cdr next-link))
        (let* ((parse (org-element-link-parser))
               (path (org-element-property :path parse))
               (end (org-element-property :end parse)))
          (if (org-zotxt-extract-link-id-from-path path)
              (org-zotxt-update-reference-link-at-point))
          (goto-char end))
        (setq next-link (org-element-link-successor))))))

(defun org-zotxt-get-item-link-text-deferred (item)
  "Get the link text for ITEM.
May be either an easy key or bibliography, depending on the value
of `org-zotxt-link-description-style'."
  (if (or (eq org-zotxt-link-description-style :easykey)
          (eq org-zotxt-link-description-style :betterbibtexkey))
      (zotxt-get-item-deferred item org-zotxt-link-description-style)
    (zotxt-get-item-bibliography-deferred item)))

(defun org-zotxt-insert-reference-link (arg)
  "Insert a zotero link in the org-mode document. Prompts for
search to choose item. If prefix argument (C-u) is used, will
insert the currently selected item from Zotero."
  (interactive "P")
  (lexical-let ((mk (point-marker)))
    (deferred:$
      (if arg 
          (zotxt-get-selected-items-deferred)
        (zotxt-choose-deferred))
      (deferred:nextc it
        (lambda (items)
          (if (null items)
              (error "No item found for search")
            (zotxt-mapcar-deferred #'org-zotxt-get-item-link-text-deferred items))))
      (deferred:nextc it
        (lambda (items)
          (with-current-buffer (marker-buffer mk)
            (goto-char (marker-position mk))
            (org-zotxt-insert-reference-links-to-items items))))
      (deferred:error it
        (lambda (err)
          (error (error-message-string err))))
      (if zotxt--debug-sync (deferred:sync! it)))))

(org-add-link-type "zotero"
                   (lambda (rest)
                     (zotxt-select-key (substring rest 15)))
                   (lambda (path desc format)
                     (if (string-match "^@\\(.*\\)$" desc)
                         (cond ((eq format 'latex)
                                (format "\\cite{%s}" (match-string 1 desc)))
                               ((eq format 'md)
                                desc)
                               (t nil))
                       nil)))

(defvar org-zotxt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c \" i") 'org-zotxt-insert-reference-link)
    (define-key map (kbd "C-c \" u") 'org-zotxt-update-reference-link-at-point)
    (define-key map (kbd "C-c \" a") 'org-zotxt-open-attachment)
    map))

(defun org-zotxt-choose-path (paths)
  "Prompt user to select a path from the PATHS.
If only path is available, return it.  If no paths are available, error."
  (if (= 0 (length paths))
      (progn (message "No attachments for item!")
             (error "No attachments for item!"))
    (if (= 1 (length paths))
        (elt paths 0)
      (zotxt-completing-read "File: " (append paths nil)))))

(defun org-zotxt-open-attachment (arg)
  "Open attachment of Zotero items linked at point.
If multiple attachments, will prompt.
With optional prefix argument ARG, Emacs will visit the file.
With a double C-u C-u prefix arg, Org tries to avoid opening in Emacs
and to use an external application to visit the file."
  (interactive "P")
  (lexical-let ((item-id (org-zotxt-extract-link-id-at-point))
                (arg arg))
    (deferred:$
      (request-deferred
       (format "%s/items" zotxt-url-base)
       :params `(("key" . ,item-id) ("format" . "recoll"))
       :parser 'json-read)
      (deferred:nextc it
        (lambda (response)
          (let ((paths (cdr (assq 'paths (elt (request-response-data response) 0)))))
            (message "%s" arg)
            (org-open-file (org-zotxt-choose-path paths) arg))))
      (if zotxt--debug-sync (deferred:sync! it)))))

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
