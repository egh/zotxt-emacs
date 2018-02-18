;;; org-zotxt.el --- Interface org-mode with Zotero via the zotxt extension

;; Copyright (C) 2010-2016 Erik Hetzner

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

(eval-when-compile
  (require 'cl))
(require 'request-deferred)
(require 'org-element)
(require 'zotxt)

(defcustom org-zotxt-link-description-style
  :citation
  "Style to use for org zotxt link texts."
  :group 'org-zotxt
  :type '(choice (const :tag "easykey" :easykey)
                 (const :tag "better BibTeX" :betterbibtexkey)
                 (const :tag "citation" :citation)))

(defcustom org-zotxt-default-search-method nil
  "Default method to use for searching with `org-zotxt-insert-reference-link'.
If nil, the user is prompted to choose each time.

A selected default method can be bypassed by giving a double
prefix argument (C-u C-u) to `org-zotxt-insert-reference-link'"
  :group 'org-zotxt
  :type (append '(choice) '((const :tag "Choose each time" nil))
                (mapcar
                 (lambda (c) (list 'const :tag (car c) (cdr c)))
                 zotxt-quicksearch-method-names)))

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
May be either an easy key or bibliography, depending on the value
of `org-zotxt-link-description-style'."
  (if (or (eq org-zotxt-link-description-style :easykey)
          (eq org-zotxt-link-description-style :betterbibtexkey))
      (zotxt-get-item-deferred item org-zotxt-link-description-style)
    (zotxt-get-item-bibliography-deferred item)))

(defun org-zotxt-insert-reference-link (&optional arg)
  "Insert a zotero link in the `org-mode' document.

Prompts for search to choose item.  If prefix argument ARG is used,
will insert the currently selected item from Zotero.  If double
prefix argument is used the search method will have to be
selected even if `org-zotxt-default-search-method' is non-nil"
  (interactive "P")
  (lexical-let ((mk (point-marker)))
    (deferred:$
      (if (equal '(4) arg)
          (zotxt-get-selected-items-deferred)
        (zotxt-choose-deferred (unless (equal '(16) arg) org-zotxt-default-search-method)))
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
  (lexical-let ((item-id (org-zotxt-extract-link-id-at-point))
                (arg arg))
    (deferred:$
      (request-deferred
       (format "%s/items" zotxt-url-base)
       :params `(("key" . ,item-id) ("format" . "paths"))
       :parser 'json-read)
      (deferred:nextc it
        (lambda (response)
          (let ((paths (cdr (assq 'paths (elt (request-response-data response) 0)))))
            (org-open-file (org-zotxt-choose-path paths) arg))))
      (if zotxt--debug-sync (deferred:sync! it)))))

(defun org-zotxt-noter (arg)
  "Like `org-noter', but use Zotero.

If no document path propery is found, will prompt for a Zotero
search to choose an attachment to annotate, then calls `org-noter'.

If a document path property is found, simply call `org-noter'."
  (interactive "P")
  (when (and (eq major-mode 'org-mode)
             (boundp 'org-noter-property-doc-file)
             (fboundp 'org-noter))
    (when (org-before-first-heading-p)
      (error "`org-zotxt-noter' must be issued inside a heading"))
    (let* ((document-property (org-entry-get nil org-noter-property-doc-file (not (equal arg '(4)))))
           (document-path (when (stringp document-property) (expand-file-name document-property))))
      (if (and document-path (not (file-directory-p document-path)) (file-readable-p document-path))
          (org-noter arg)
        (lexical-let ((arg arg))
          (deferred:$
            (zotxt-choose-deferred)
            (deferred:nextc it
              (lambda (item-ids)
                (zotxt-get-item-deferred (car item-ids) :paths)))
            (deferred:nextc it
              (lambda (resp)
                (let ((path (org-zotxt-choose-path (cdr (assq 'paths (plist-get resp :paths))))))
                  (org-entry-put nil org-noter-property-doc-file path))
                (org-noter arg)))))))))

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
  org-zotxt-mode-map
  (org-zotxt--define-links))

(provide 'org-zotxt)
;;; org-zotxt.el ends here
