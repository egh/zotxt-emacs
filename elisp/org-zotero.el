(require 'org)
(require 'zotero)

(defun org-zotero-update-reference-link-at-point ()
  (interactive)
  (save-excursion
    (if (not (looking-at "\\[\\["))
        (re-search-backward "\\[\\["))
    (re-search-forward "\\([A-Z0-9_]+\\)\\]\\[")
    (let* ((item-id (match-string 1))
           (start (point))
           (text (zotero-generate-bib-entry-from-id item-id)))
      (re-search-forward "\\]\\]\\|$")
      (delete-region start (point))
      (insert text)
      (insert "]]"))))

(defun org-zotero-insert-reference-link ()
  (interactive)
  (let ((ids (zotero-get-selected-item-ids)))
    (mapc (lambda (id)
            (insert (format
                     "[[zotero://select/items/%s][%s]]\n"
                     id id))
            (org-zotero-update-reference-link-at-point)
            (forward-line 1))
          ids)))

(org-add-link-type "zotero"
                   (lambda (rest)
                     (browse-url (format "zotero:%s" rest))))

(defvar org-zotero-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control c) (z) (i)] 'org-zotero-insert-reference-link)
    (define-key map [(control c) (z) (u)] 'org-zotero-update-reference-link-at-point)
    map))

(define-minor-mode org-zotero-mode
  "Toggle org-zotero-mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

This is a minor mode for managing your citations with Zotero in a
org-mode document."  
  nil
  "Zotero"
  org-zotero-mode-map)

(defun org-zotero-find-reference-create (id)
  (widen)
  (goto-char (point-min))
  (if (not (re-search-forward (format "^\\* \\[\\[zotero://select//%s\\]" id) nil t))
      (progn
        (goto-char (point-max))
        (insert (format
                 "\n* [[zotero://select//%s][%s]]\n"
                 id id))
        (forward-line -1)
        (org-zotero-update-reference-link-at-point)
        (forward-line 1))))

(defun org-zotero-insert-note (id note)
  (org-zotero-find-reference-create id)
  (org-forward-same-level 1)
  (insert note)
  (insert "\n"))

(provide 'org-zotero)
