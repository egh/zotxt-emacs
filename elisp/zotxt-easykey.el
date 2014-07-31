;;; zotxt-easykey.el --- Zotero easykey support via the zotxt extension
     
;; Copyright (C) 2010-2014 Erik Hetzner

;; Author: Erik Hetzner <egh@e6h.org>
;; Keywords: bib

;; This file is not part of GNU Emacs.

;; zotxt-easykey.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; zotxt-easykey.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with zotxt-easykey.el. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'zotxt)

(defvar zotxt-easykey-regex
  "[@{]\\([[:alnum:]:]+\\)")

(defvar zotxt-easykey-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c \" o") 'zotxt-easykey-select-item-at-point)
    (define-key map (kbd "C-c \" k") 'zotxt-easykey-insert)
    map))

(defun zotxt-easykey-at-point-match ()
  (or (looking-at zotxt-easykey-regex)
      (save-excursion
        ;; always try to back up one char
        (backward-char)
        (while (and (not (looking-at zotxt-easykey-regex))
                    (looking-at "[[:alnum:]:]"))
          (backward-char))
        (looking-at zotxt-easykey-regex))))

(defun zotxt-easykey-at-point ()
  "Return the value of the easykey at point. Easykey must start
with a @ or { to be recognized, but this will *not* be returned."
  (save-excursion
    (if (zotxt-easykey-at-point-match)
        (match-string 1)
      nil)))
  
(defun zotxt-easykey-complete-at-point ()
  (save-excursion
    (if (not (zotxt-easykey-at-point-match))
        nil
      (let ((start (match-beginning 0))
            (end (match-end 0))
            (key (match-string 1)))
        (let* ((url (format "http://127.0.0.1:23119/zotxt/complete?easykey=%s" key))
               (response (zotxt-url-retrieve url)))
          (if (null response)
              nil
            (let ((completions (mapcar (lambda (k) (format "@%s" k)) response)))
              (list start end completions))))))))

(defun zotxt-easykey-get-item-id-at-point ()
  "Return the Zotero ID of the item referred to by the easykey at
point, or nil."
  (save-excursion
    (let ((key (zotxt-easykey-at-point)))
      (if (null key)
          nil
        (let* ((url (format "http://127.0.0.1:23119/zotxt/items?format=key&easykey=%s" key))
               (response (zotxt-url-retrieve url)))
          (if (null response)
              nil
            (elt response 0)))))))

(defun zotxt-easykey-get-item-easykey (key)
  (elt (zotxt-url-retrieve
        (format "http://127.0.0.1:23119/zotxt/items?key=%s&format=easykey" key)) 0))

(defun zotxt-easykey-insert (arg)
  "Prompt for a search string and insert an easy key. With C-u,
insert easykeys for the currently selected items in Zotero."
  (interactive "P")
  (let ((keys (if arg
                 (zotxt-get-selected-item-ids)
                (list (cdr (zotxt-choose))))))
    (insert (mapconcat (lambda (key)
                         (format "@%s" (zotxt-easykey-get-item-easykey key))) keys " "))))

(defun zotxt-easykey-select-item-at-point ()
  "Select the item referred to by the easykey at point in
Zotero."
  (interactive)
  (zotxt-select-easykey (zotxt-easykey-at-point)))

;;;###autoload
(define-minor-mode zotxt-easykey-mode
  "Toggle zotxt-easykey-mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

This is a minor mode for managing your easykey citations,
including completion."
  :init-value nil
  :lighter " ZotEasykey"
  :keymap zotxt-easykey-mode-map
  (if zotxt-easykey-mode
      (setq-local completion-at-point-functions
                  (cons 'zotxt-easykey-complete-at-point
                        completion-at-point-functions))
    (setq-local completion-at-point-functions
                (remove 'zotxt-easykey-complete-at-point 
                        completion-at-point-functions))))

(provide 'zotxt-easykey)
;;; zotxt-easykey.el ends here
