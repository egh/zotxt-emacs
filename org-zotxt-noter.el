;; -*- lexical-binding: t -*-
;;; org-zotxt-noter.el --- Zotxt support for org-noter

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

(require 'deferred)
(require 'org-noter)

(defun org-zotxt-noter (arg)
  "Like `org-noter', but use Zotero.

If no document path propery is found, will prompt for a Zotero
search to choose an attachment to annotate, then calls `org-noter'.

If a document path property is found, simply call `org-noter'.

See `org-noter' for details and ARG usage."
  (interactive "P")
  (require 'org-noter nil t)
  (unless (eq major-mode 'org-mode)
    (error "Org mode not running"))
  (unless (fboundp 'org-noter)
    (error "`org-noter' not installed"))
  (if (org-before-first-heading-p)
      (error "`org-zotxt-noter' must be issued inside a heading"))
  (let* ((document-property (org-entry-get nil org-noter-property-doc-file (not (equal arg '(4)))))
         (document-path (when (stringp document-property) (expand-file-name document-property))))
    (if (and document-path (not (file-directory-p document-path)) (file-readable-p document-path))
        (call-interactively #'org-noter)
      (let ((arg arg))
        (deferred:$
          (zotxt-choose-deferred)
          (deferred:nextc it
            (lambda (item-ids)
              (zotxt-get-item-deferred (car item-ids) :paths)))
          (deferred:nextc it
            (lambda (item)
              (org-zotxt-get-item-link-text-deferred item)))
          (deferred:nextc it
            (lambda (resp)
              (let ((path (org-zotxt-choose-path (cdr (assq 'paths (plist-get resp :paths))))))
                (org-entry-put nil org-zotxt-noter-zotero-link (org-zotxt-make-item-link resp))
                (org-entry-put nil org-noter-property-doc-file path))
              (call-interactively #'org-noter)))
          (deferred:error it #'zotxt--deferred-handle-error))))))

(provide 'org-zotxt-noter)
;;; org-zotxt-noter.el ends here
