(require 'zotero)

(defun html-to-rst (input)
  (let* ((doc
          (with-temp-buffer
            (insert input)
            (xml-parse-region (point-min) (point-max))))
         (pr-list (lambda (lst)
                    (if (listp lst)
                        (mapc pr lst)
                      (funcall pr lst))))
         (pr (lambda (node)
               (if (stringp node)
                   (insert node)
                 (let ((qname (car node))
                       (attrs (car (cdr node)))
                       (children (cdr (cdr node))))
                   (cond ((eq qname 'span)
                          (let ((style (cdr (assq 'style attrs))))
                            (if (string= style "font-style:italic;")
                                (progn (insert "*")
                                       (funcall pr-list children)
                                       (insert "*")))))
                         (t
                          (funcall pr-list children))))))))
    (with-temp-buffer
      (funcall pr-list doc)
      (buffer-string))))

(defun zotero-rst-delete-reference-body-at-point ()
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at "[[:blank:]]*\\.\\. zotero:: \\([[:alnum:]_]+\\)"))
        (error "Not at citation!")
      (progn
        (re-search-forward "zotero:: \\([[:alnum:]_]+\\)")
        (let ((beg (point)))
          (end-of-line)
          (delete-region beg (point)))
        (forward-line 1)
        (beginning-of-line)
        (while (looking-at " ")
          (kill-whole-line))))))

(defun zotero-rst-update-reference-body-at-point ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at "[[:blank:]]*\\.\\. zotero:: \\([[:alnum:]_]+\\)"))
        (error "Not at citation!")
      (progn
        (zotero-rst-delete-reference-body-at-point)
        (re-search-forward "zotero:: \\([[:alnum:]_]+\\)")
        (let ((bib-id (match-string 1)))
          (insert " ")
          (insert (html-to-rst (zotero-generate-bib-entry-from-id 
                                bib-id
                                zotero-default-bibliography-style
                                "html"))))))))

(defun zotero-rst-insert-reference ()
  (interactive)
  (let ((ids (zotero-get-selected-item-ids)))
    (mapc (lambda (id)
            (if (string-match "^0_" id)
                (setq id (substring id 2)))
            (insert (format ".. [%s]\n" id))
            (insert (format "  .. zotero:: %s\n" id))
            (forward-line -1)
            (zotero-rst-update-reference-body-at-point)
            (forward-line 1))
          ids)))

(defvar zotero-rst-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control c) (z) (i)] 'zotero-rst-insert-reference)
    (define-key map [(control c) (z) (u)] 'zotero-rst-update-reference-body-at-point)
    map))

(define-minor-mode zotero-rst-mode
  "Toggle zotero-rst-mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

This is a minor mode for managing your citations with Zotero in a
reStructuredText document."
  nil
  "Zotero"
  zotero-rst-mode-map)

(provide 'zotero-rst)