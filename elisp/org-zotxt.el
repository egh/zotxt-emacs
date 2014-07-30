(require 'org)
(require 'zotxt)

(defun org-zotxt-update-reference-link-at-point ()
  "Updates the zotero:// link at point."
  (interactive)
  (save-excursion
    (if (not (looking-at "\\[\\["))
        (re-search-backward "\\[\\["))
    (re-search-forward "\\([A-Z0-9_]+\\)\\]\\[")
    (let* ((item-id (match-string 1))
           (start (point))
           (text (zotxt-generate-bib-entry-from-id item-id)))
      (re-search-forward "\\]\\]\\|$")
      (delete-region start (point))
      (insert text)
      (insert "]]"))))

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
          (if (string-match "^zotero" path)
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

(defun org-zotxt-insert-attachment-link ()
  (interactive)
  (let* ((key (cdr (zotxt-choose)))
         (item (zotxt-get-item key "recoll"))
         (paths (cdr (assq 'paths (elt item 0)))))
    (if (= 0 (length paths))
        (error "No attachments for selected item!")
      (if (= 1 (length paths))
          (org-zotxt-insert-attachment-link-to-path (elt paths 0))
        (org-zotxt-insert-attachment-link-to-path
         (completing-read "File: " (append paths nil)))))))

(defun org-zotxt-insert-attachment-link-to-path (path)
  (let* ((escaped-path (org-link-escape (expand-file-name path)))
         (link-type (if (string-match "\\(pdf\\|dvi\\|ps\\|docx\\|odt\\)$" escaped-path)
                        "docview"
                      "file"))
         (page-number (if (string= link-type "docview")
                          (concat "::" (read-from-minibuffer "Page:" "1"))
                        "")))
    (insert (format "%s:%s%s" link-type escaped-path page-number))))
    
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
