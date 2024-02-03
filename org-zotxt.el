;; -*- lexical-binding: t -*-
;;; org-zotxt.el --- Interface org-mode with Zotero via the zotxt extension

;; Copyright (C) 2010-2020 Erik Hetzner

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

(require 'org-element)
(require 'zotxt)
(require 'deferred)

(defcustom org-zotxt-link-description-style
  :citation
  "Style to use for org zotxt link texts."
  :group 'org-zotxt
  :type '(choice (const :tag "citekey" :citekey)
                 (const :tag "citation" :citation)
                 (const :tag "title" :title)))

(defcustom org-zotxt-link-insert-newline t
  "Whether or not to insert a newline after a link.
See `org-zotxt-insert-reference-link'."
  :group 'org-zotxt
  :type 'boolean)

(make-obsolete-variable 'org-zotxt-default-search-method 'zotxt-default-search-method "6.0")

(defcustom org-zotxt-noter-zotero-link "ZOTERO_LINK"
  "Default property name for zotero link."
  :group 'zotxt
  :type 'string)

(defun org-zotxt-extract-link-id-at-point ()
  "Extract the Zotero key of the link at point."
  (let ((ct (org-element-context)))
    (if (eq 'link (org-element-type ct))
        (org-zotxt-extract-link-id-from-path (org-element-property :path ct))
      nil)))

(defun org-zotxt-extract-link-id-from-path (path)
  "Return the zotxt ID from a link PATH."
  (if (string-match "^\\(zotero:\\)?//select/items/\\(.*\\)$" path)
      (match-string 2 path)
    nil))

(defun org-zotxt-make-item-link (item)
  "Return an Org mode link for ITEM as a string."
  (org-make-link-string
   (format "zotero://select/items/%s"
           (plist-get item :key))
   (cl-case org-zotxt-link-description-style
     (:citekey (concat "@" (plist-get item org-zotxt-link-description-style)))
     (:title (plist-get item :title))
     (t (plist-get item :citation)))))

(defun org-zotxt-insert-reference-link-to-item (item)
  "Insert link to Zotero ITEM in buffer."
  (insert (org-zotxt-make-item-link item)))

(defun org-zotxt-insert-reference-links-to-items (items)
  "Insert links to Zotero ITEMS in buffer."
  (mapc (lambda (item)
          (org-zotxt-insert-reference-link-to-item item)
	  (when org-zotxt-link-insert-newline
            (insert "\n")
            (forward-line 1)))
        items))

(defun org-zotxt-update-reference-link-at-point ()
  "Update the zotero:// link at point."
  (interactive)
  (let ((mk (point-marker))
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
                                   (- (org-element-property :end ct)
                                      (org-element-property :post-blank ct)))
                    (org-zotxt-insert-reference-link-to-item item))))))
          (deferred:error it #'zotxt--deferred-handle-error)
          (if zotxt--debug-sync (deferred:sync! it))))))

(defun org-zotxt-update-all-reference-links ()
  "Update all zotero:// links in a document."
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-max))
    (while (re-search-backward org-any-link-re nil t)
      (let* ((parse (org-element-link-parser))
             (path (org-element-property :path parse)))
        (when (org-zotxt-extract-link-id-from-path path)
          (message "[zotxt] updating path: %s" path)
          (org-zotxt-update-reference-link-at-point))))))

(defun org-zotxt-get-item-link-text-deferred (item)
  "Get the link text for ITEM.
May be either an citekey or bibliography, depending on the value
of `org-zotxt-link-description-style'."
  (let ((item (copy-tree item)))
   (cl-case org-zotxt-link-description-style
     (:citekey (zotxt-get-item-deferred item org-zotxt-link-description-style))
     (:title (deferred:callback-post
               (deferred:new)
               (plist-put item
                          :title (zotxt-key-to-title (plist-get item :key)))))
     (t (zotxt-get-item-bibliography-deferred item)))))

(defun org-zotxt-insert-reference-link (&optional arg)
  "Insert a zotero link in the `org-mode' document.

Prompts for search to choose item.  If prefix argument ARG is used,
will insert the currently selected item from Zotero.  If double
prefix argument is used the search method will have to be
selected even if `org-zotxt-default-search-method' is non-nil"
  (interactive "p")
  (let ((mk (point-marker))
        (use-current-selected (equal '(4) arg))
        (force-choose-search-method (equal '(16) arg)))
    (deferred:$
      (zotxt-choose-deferred arg)
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
      (deferred:error it #'zotxt--deferred-handle-error)
      (if zotxt--debug-sync (deferred:sync! it)))))

(defun org-zotxt--link-follow (path)
  "Function used for zotero links to follow the link to PATH."
  (zotxt-select-key (substring path 15)))

(defun org-zotxt--link-export (path desc format)
  "Function used for zotero links to export the link.

PATH is the path of the link, the text after the prefix (like \"http:\")
DESC is the description of the link, if any
FORMAT is the export format, a symbol like ‘html’ or ‘latex’ or ‘ascii’."
  (if (string-match "^@\\(.*\\)$" desc)
      (pcase format
        (`latex (format "\\cite{%s}"
                        ;; hack to replace all the escaping that latex
                        ;; gives us in the desc with _
                        (replace-regexp-in-string
                         "\\([{}]\\|\\\\text\\|\\\\(\\|\\\\)\\)" ""
                         (match-string 1 desc))))
        (`md desc)
        (_ nil))))

(defvar org-zotxt--links-defined nil)

;; We need to support org 9 and org 8, but this code will generate compiler
;; warnings without this
(with-no-warnings
  (defun org-zotxt--define-links ()
    "Set up the links for zotxt."
    (when (not org-zotxt--links-defined)
      (setq org-zotxt--links-defined t)
      (if (functionp #'org-link-set-parameters)
          (org-link-set-parameters "zotero"
                                   :follow #'org-zotxt--link-follow
                                   :export #'org-zotxt--link-export)
        (org-add-link-type "zotero"
                           #'org-zotxt--link-follow
                           #'org-zotxt--link-export)))))

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
      (completing-read "File: " (append paths nil)))))

(defun org-zotxt-open-attachment (&optional arg)
  "Open attachment of Zotero items linked at point.

Opens with `org-open-file', see for more information about ARG."
  (interactive "P")
  (let ((item-id (org-zotxt-extract-link-id-at-point))
        (arg arg))
    (deferred:$
      (zotxt--request-deferred
       (format "%s/items" zotxt-url-base)
       :params `(("key" . ,item-id) ("format" . "paths"))
       :parser 'json-read)
      (deferred:nextc it
        (lambda (response)
          (let ((paths (cdr (assq 'paths (elt (request-response-data response) 0)))))
            (org-open-file (org-zotxt-choose-path paths) arg))))
      (deferred:error it #'zotxt--deferred-handle-error)
      (if zotxt--debug-sync (deferred:sync! it)))))

;;;###autoload
(define-minor-mode org-zotxt-mode
  "Toggle org-zotxt-mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

This is a minor mode for managing your citations with Zotero in a
org-mode document."
  :init-value nil
  :lighter " OrgZot"
  :keymap org-zotxt-mode-map
  (org-zotxt--define-links))

(provide 'org-zotxt)
;;; org-zotxt.el ends here
