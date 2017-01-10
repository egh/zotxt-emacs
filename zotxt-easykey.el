;;; zotxt-easykey.el --- Interface emacs with Zotero via the zotxt extension

;; Copyright (C) 2010-2017 Erik Hetzner

;; Author: Erik Hetzner <egh@e6h.org>
;; Keywords: bib
;; Version: 0.1.35

;; This file is not part of GNU Emacs.

;; zotxt.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; zotxt.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with zotxt.el. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'zotxt)
(require 'request-deferred)

(defvar zotxt-easykey-regex
  "[@{]\\([[:alnum:]:]+\\)")

(defvar zotxt-easykey-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c \" o") 'zotxt-easykey-select-item-at-point)
    (define-key map (kbd "C-c \" k") 'zotxt-easykey-insert)
    map))

(defun zotxt-easykey-at-point-match ()
  "Match an easykey at point."
  (or (looking-at zotxt-easykey-regex)
      (save-excursion
        ;; always try to back up one char
        (backward-char)
        (while (and (not (looking-at zotxt-easykey-regex))
                    (looking-at "[[:alnum:]:]"))
          (backward-char))
        (looking-at zotxt-easykey-regex))))

(defun zotxt-easykey-at-point ()
  "Return the value of the easykey at point.

Easykey must start with a @ or { to be recognized, but this will
not be returned."
  (save-excursion
    (if (zotxt-easykey-at-point-match)
        (match-string 1)
      nil)))

(defun zotxt-easykey-complete-at-point ()
  "Complete the easykey at point."
  (save-excursion
    (if (not (zotxt-easykey-at-point-match))
        nil
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (key (match-string 1))
             (completions
              (deferred:$
                (request-deferred
                 (format "%s/complete" zotxt-url-base)
                 :params `(("easykey" . ,key))
                 :parser #'zotxt--json-read)
                (deferred:nextc it
                  (lambda (response)
                    (mapcar (lambda (k) (format "@%s" k))
                            (request-response-data response))))
                (deferred:sync! it))))
        (if (null completions)
            nil
          (list start end completions))))))

(defun zotxt-easykey-insert (&optional selected)
  "Prompt for a search string and insert an easy key.

If SELECTED is non-nill (interactively, With prefix argument), insert easykeys for the currently selected items in Zotero."
  (interactive (if current-prefix-arg t))
  (lexical-let ((mk (point-marker)))
    (deferred:$
      (if selected
          (zotxt-get-selected-items-deferred)
        (zotxt-choose-deferred))
      (deferred:nextc it
        (lambda (items)
          (zotxt-mapcar-deferred (lambda (item)
                                   (zotxt-get-item-deferred item :easykey))
                                 items)))
      (deferred:nextc it
        (lambda (items)
          (with-current-buffer (marker-buffer mk)
            (goto-char (marker-position mk))
            (insert (mapconcat
                     (lambda (item)
                       (format "@%s" (plist-get item :easykey)))
                     items " ")))))
      (if zotxt--debug-sync (deferred:sync! it)))))

(defun zotxt-easykey-select-item-at-point ()
  "Select the item referred to by the easykey at point in Zotero."
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
