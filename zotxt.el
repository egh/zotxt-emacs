;;; zotxt.el --- Interface emacs with Zotero via the zotxt extension

;; Copyright (C) 2010-2016 Erik Hetzner

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

(eval-when-compile
  (require 'cl))
(require 'json)
(require 'request-deferred)

(defvar zotxt-default-bibliography-style
  "chicago-note-bibliography"
  "Default bibliography style to use.")

(defconst zotxt-url-base
  "http://127.0.0.1:23119/zotxt"
  "Base URL to contact.")

(defconst zotxt--json-formats
  '(:easykey :betterbibtexkey :json :paths :quickBib)
  "Formats to parse as JSON.")

(defvar zotxt--debug-sync nil
  "Use synchronous requests.  For debug only!")

(defun zotxt-mapcar-deferred (func lst)
  "Apply FUNC (which must return a deferred object), to each element of LST.

Will pass on a list of results.
Runs in parallel using `deferred:parallel'."
  (apply #'deferred:parallel
         (mapcar func lst)))

(defun zotxt--json-read ()
  "UTF-8 aware `json-read'.

request.el is not decoding our responses as UTF-8.  Recode text as UTF-8 and parse."
  (recode-region (point-min) (point-max) 'utf-8 'raw-text)
  (json-read))

(defun zotxt-make-quick-bib-string (item)
  "Make a useful quick bibliography string from ITEM."
  (let* ((json (plist-get item :json))
         (author (cdr (assq 'author json)))
         (title (cdr (assq 'title json)))
         (author-string (if (= (length author) 0)
                            ""
                          (let* ((first (elt author 0))
                                 (given (cdr (assq 'given first)))
                                 (family (cdr (assq 'family first))))
                            (format "%s, %s" family given)))))
    (format "%s - %s" author-string title)))

(defun zotxt--id2key (id)
  "Turn an ID, as returned by Zotero, into a key."
  (if (string-match "/\\([^/]+\\)$" id)
      (format "0_%s" (match-string 1 id))))

(defun zotxt-get-item-bibliography-deferred (item)
  "Retrieve the generated bibliography for ITEM (a plist).
Use STYLE to specify a custom bibliography style.
Adds a plist entry with the name of the style as a self-quoting symbol, e.g.
:chicago-note-bibliography.
Also adds :citation entry if STYLE is the default.
Also adds HTML versions, suffixed with -html.

For use only in a `deferred:$' chain."
  (lexical-let ((d (deferred:new))
                (style zotxt-default-bibliography-style)
                (item item))
    (if (and (string= style zotxt-default-bibliography-style)
             (plist-get item :citation))
        ;; item already has citation, no need to fetch
        (deferred:callback-post d item)
      (request
       (format "%s/items" zotxt-url-base)
       :params `(("key" . ,(plist-get item :key))
                 ("format" . "bibliography")
                 ("style" . ,style))
       :parser #'zotxt--json-read
       :success (function*
                 (lambda (&key data &allow-other-keys)
                   (let* ((style-key (intern (format ":%s" style)))
                          (style-key-html (intern (format ":%s-html" style)))
                          (first (elt data 0))
                          (text (cdr (assq 'text first)))
                          (html (cdr (assq 'html first))))
                     (if (string= style zotxt-default-bibliography-style)
                         (progn
                           (plist-put item :citation text)
                           (plist-put item :citation-html html)))
                     (plist-put item style-key text)
                     (plist-put item style-key-html html)
                     (deferred:callback-post d item))))))
    d))

(defun zotxt-get-selected-items-deferred ()
  "Return the currently selected items in Zotero.

For use only in a `deferred:$' chain."
  (lexical-let ((d (deferred:new)))
    (request
     (format "%s/items" zotxt-url-base)
     :params '(("selected" . "selected")
               ("format" . "key"))
     :parser #'zotxt--json-read
     :success (function*
               (lambda (&key data &allow-other-keys)
                     (deferred:callback-post
                       d (mapcar (lambda (k)
                                   (list :key k))
                                 data)))))
      d))

(defconst zotxt-quicksearch-method-names
  '(("title, creator, year" . :title-creator-year)
    ("fields" . :fields)
    ("everything" . :everything)))

(defconst  zotxt-quicksearch-method-params
  '((:title-creator-year . "titleCreatorYear")
    (:fields . "fields")
    (:everything . "everything")))

(defconst zotxt-quicksearch-method-to-names
  '((:title-creator-year . "title, creator, year")
    (:fields . "fields")
    (:everything . "everything")))

(defun zotxt-choose-deferred (&optional method search-string)
  "Allow the user to select an item interactively.

If METHOD is supplied, it should be one
of :title-creator-year, :fields, or :everything.
If SEARCH-STRING is supplied, it should be the search string."
  (if (null method)
      (let ((method-name
             (completing-read
              "Zotero search method (nothing for title, creator, year): "
              zotxt-quicksearch-method-names
              nil t nil nil "title, creator, year")))
        (setq method (cdr (assoc method-name zotxt-quicksearch-method-names)))))
  (if (null search-string)
      (setq search-string
            (read-string (format "Zotero quicksearch (%s) query: " (cdr (assq method zotxt-quicksearch-method-to-names))))))
  (lexical-let ((d (deferred:new)))
    (request
     (format "%s/search" zotxt-url-base)
     :params `(("q" . ,search-string)
               ("method" . ,(cdr (assq method zotxt-quicksearch-method-params)))
               ("format" . "quickBib"))
     :parser #'zotxt--json-read
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (let* ((results (mapcar (lambda (e)
                                           (cons (cdr (assq 'quickBib e))
                                                 (cdr (assq 'key e))))
                                         data))
                        (count (length results))
                        (citation (if (= 0 count)
                                      nil
                                    (if (= 1 count)
                                        (car (car results))
                                      (completing-read "Select item: " results))))
                        (key (cdr (assoc-string citation results))))
                   (deferred:callback-post
                     d (if (null citation) nil
                         `((:key ,key))))))))
    d))

(defun zotxt-select-easykey (easykey)
  "Select the item identified by EASYKEY in Zotero."
  (request
   (format "%s/select" zotxt-url-base)
   :params `(("easykey" . ,easykey))))

(defun zotxt-select-key (key)
  "Select the item identified by KEY in Zotero."
  (request
   (format "%s/select" zotxt-url-base)
   :params `(("key" . ,key))))

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

(defun zotxt-get-item-deferred (item format)
  "Given a plist ITEM, add the FORMAT.

For use only in a `deferred:$' chain."
  (lexical-let ((item item)
                (format format)
                (d (deferred:new)))
    (request
     (format "%s/items" zotxt-url-base)
     :params `(("key" . ,(plist-get item :key))
               ("format" . ,(substring (symbol-name format) 1)))
     :parser (if (member format zotxt--json-formats)
                 #'zotxt--json-read
               #'buffer-string)
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (if (member format zotxt--json-formats)
                     ;; json data
                     (plist-put item format (elt data 0))
                   (plist-put item format data))
                 (deferred:callback-post d item))))
    d))

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

(provide 'zotxt)
;;; zotxt.el ends here
