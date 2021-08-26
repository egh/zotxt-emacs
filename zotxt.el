;; -*- lexical-binding: t -*-
;;; zotxt.el --- Interface emacs with Zotero via the zotxt extension

;; Copyright (C) 2010-2020 Erik Hetzner

;; Author: Erik Hetzner <egh@e6h.org>
;; Keywords: bib
;; Version: 5.0.5
;; Package-Requires: ((emacs "24.3"))

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

(require 'cl-macs)
(require 'json)
(require 'deferred)
(require 'request)

(defconst zotxt-url-base
  "http://127.0.0.1:23119/zotxt"
  "Base URL to contact.")

(defconst zotxt-quicksearch-method-params
  '((:title-creator-year . "titleCreatorYear")
    (:fields . "fields")
    (:everything . "everything")))

(defconst zotxt--json-formats
  '(:citekey :json :paths :quickBib)
  "Formats to parse as JSON.")

(defconst zotxt-quicksearch-method-names
  '(("title, creator, year" . :title-creator-year)
    ("fields" . :fields)
    ("everything" . :everything)))

(defconst zotxt-quicksearch-method-to-names
  '((:title-creator-year . "title, creator, year")
    (:fields . "fields")
    (:everything . "everything")))

(defvar zotxt--debug-sync nil
  "Use synchronous requests.  For debug only!")

(defcustom zotxt-default-bibliography-style
  "chicago-note-bibliography"
  "Default bibliography style to use. Should be named after the style file name, e.g. apa, ieee, chicago-note-bibliography. Sorry, there should be a better way to get the name!"
  :group 'zotxt
  :type '(string))

(defcustom zotxt-default-library :all
  "Default library to search. :all for all libraries, :user for user library."
  :group 'zotxt
  :type '(choice (const :tag "Search all libraries" :all)
                 (const :tag "User library" :all)))

(defcustom zotxt-default-search-method nil
  "Default method to use for searching for items.
If nil, the user is prompted to choose each time."

  :group 'zotxt
  :type (append '(choice) '((const :tag "Choose each time" nil))
                (mapcar
                 (lambda (c) (list 'const :tag (car c) (cdr c)))
                 zotxt-quicksearch-method-names)))

(defun zotxt--deferred-handle-error (err)
  "Deferred chain error handler.

Prints ERR and checks if zotxt is installed."
  (message "Caught error: %S" err)
  (zotxt--check-server)
  (apply #'signal err))

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

(defun zotxt--check-server ()
  "Use the zotxt version endpoint to check if Zotero is running and zotxt is installed."
  (let* ((response
          (request
            (format "%s/version" zotxt-url-base)
            :sync t))
         (status-code (request-response-status-code response)))
    (unless (and status-code (= 200 status-code))
      (error "Zotxt version endpoint not found; is Zotero running and zotxt installed?"))))

(defun zotxt--request-deferred (url &rest args)
  (let* ((d (deferred:new #'identity))
         (callback-post (apply-partially
                         (lambda (d &rest args)
                           (deferred:callback-post
                             d (plist-get args :response)))
                         d))
         (errorback-post (apply-partially
                          (lambda (d &rest args)
                            (deferred:errorback-post
                              d (plist-get args :response)))
                          d)))
    (setq args (plist-put args :success callback-post))
    (setq args (plist-put args :error errorback-post))
    (apply #'request url args)
    d))

(defun zotxt-get-item-bibliography-deferred (item)
  "Retrieve the generated bibliography for ITEM (a plist).
Use STYLE to specify a custom bibliography style.
Adds a plist entry with the name of the style as a self-quoting symbol, e.g.
:chicago-note-bibliography.
Also adds :citation entry if STYLE is the default.
Also adds HTML versions, suffixed with -html.

For use only in a `deferred:$' chain."
  (let ((d (deferred:new))
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
        :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                              (deferred:errorback-post d error-thrown)))
        :success (cl-function
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

(defun zotxt-item-alist (item-id)
  "Return the complete alist of an item by ITEM-ID.
ITEM-ID appears to be the `<libraryId>_<key>' in the `items' table."
  (let* ((item `(:key ,item-id))
         (plist-key :full)
         (zotxt--json-formats (list plist-key)))
    (->> (zotxt-get-item-deferred item plist-key)
         (funcall (lambda (item)
                    (deferred:error item #'zotxt--deferred-handle-error)))
         deferred:sync!
         (funcall (lambda (plist)
                    (plist-get plist plist-key))))))

(defun zotxt-key-to-title (item-id)
  "Return the title of a paper identified by ITEM-ID."
  (->> (zotxt-item-alist item-id)
       (funcall (lambda (alist)
                  (or (assoc 'title-short alist) (assoc 'title alist))))
       cdr))

(defun zotxt-get-selected-items-deferred ()
  "Return the currently selected items in Zotero.

For use only in a `deferred:$' chain."
  (let ((d (deferred:new)))
    (request
      (format "%s/items" zotxt-url-base)
      :params '(("selected" . "selected")
                ("format" . "key"))
      :parser #'zotxt--json-read
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (deferred:errorback-post d error-thrown)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (deferred:callback-post
                    d (mapcar (lambda (k)
                                (list :key k))
                              data)))))
    d))

(defun zotxt--read-search-method ()
  "Prompt user for Zotero search method to use, return a symbol."
  (let ((method-name
         (completing-read
          "Zotero search method (nothing for title, creator, year): "
          zotxt-quicksearch-method-names
          nil t nil nil "title, creator, year")))
    (cdr (assoc method-name zotxt-quicksearch-method-names))))

(defun zotxt-search-deferred (&optional method search-string)
  "Allow the user to select an item interactively.

If METHOD is supplied, it should be one
of :title-creator-year, :fields, or :everything.
If SEARCH-STRING is supplied, it should be the search string."
  (if (null method)
      (setq method (zotxt--read-search-method)))
  (if (null search-string)
      (setq search-string
            (read-string (format "Zotero quicksearch (%s) query: " (cdr (assq method zotxt-quicksearch-method-to-names))))))
  (if (string-match-p "\\`\\s-*$" search-string)
      (error "Please provide search string"))
  (let ((d (deferred:new)))
    (request
      (format "%s/search" zotxt-url-base)
      :params `(("q" . ,search-string)
                ("method" . ,(cdr (assq method zotxt-quicksearch-method-params)))
                ,@(cond ((eq :all zotxt-default-library)
                         '(("library" ."all")))
                        ((eq :user zotxt-default-library)
                         nil))
                ("format" . "quickBib"))
      :parser #'zotxt--json-read
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (deferred:errorback-post d error-thrown)))
      :success (cl-function
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

(defun zotxt-choose-deferred (&optional arg)
  "Allow the user to select an item interactively.

ARG should be numberic prefix argugument from (interactive \"P\").

If universal argment was used, will insert the currently selected
item from Zotero. If double universal argument is used the search
method will have to be selected even if
`zotxt-default-search-method' is non-nil."
  (let ((use-current-selected (eq 4 arg))
        (force-choose-search-method (eq 16 arg)))
    (if use-current-selected
        (zotxt-get-selected-items-deferred)
      (zotxt-search-deferred (unless force-choose-search-method zotxt-default-search-method)))))

(defun zotxt-select-citekey (citekey)
  "Select the item identified by CITEKEY in Zotero."
  (request
    (format "%s/select" zotxt-url-base)
    :params `(("citekey" . ,citekey))))

(defun zotxt-select-key (key)
  "Select the item identified by KEY in Zotero."
  (request
    (format "%s/select" zotxt-url-base)
    :params `(("key" . ,key))))

(defvar zotxt-citekey-regex
  "[@{]\\([[:alnum:]:]+\\)")

(defvar zotxt-citekey-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c \" o") 'zotxt-citekey-select-item-at-point)
    (define-key map (kbd "C-c \" k") 'zotxt-citekey-insert)
    map))

(defun zotxt-citekey-at-point-match ()
  "Match an citekey at point."
  (or (looking-at zotxt-citekey-regex)
      (save-excursion
        ;; always try to back up one char
        (backward-char)
        (while (and (not (looking-at zotxt-citekey-regex))
                    (looking-at "[[:alnum:]:]"))
          (backward-char))
        (looking-at zotxt-citekey-regex))))

(defun zotxt-citekey-at-point ()
  "Return the value of the citekey at point.

Citekey must start with a @ or { to be recognized, but this will
not be returned."
  (save-excursion
    (if (zotxt-citekey-at-point-match)
        (match-string 1)
      nil)))

(defun zotxt-citekey-complete-at-point ()
  "Complete the citekey at point."
  (save-excursion
    (if (not (zotxt-citekey-at-point-match))
        nil
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (key (match-string 1))
             (completions
              (deferred:$
                (zotxt--request-deferred
                 (format "%s/complete" zotxt-url-base)
                 :params `(("citekey" . ,key))
                 :parser #'zotxt--json-read)
                (deferred:nextc it
                  (lambda (response)
                    (mapcar (lambda (k) (format "@%s" k))
                            (request-response-data response))))
                (deferred:error it #'zotxt--deferred-handle-error)
                (deferred:sync! it))))
        (if (null completions)
            nil
          (list start end completions))))))

(defun zotxt-get-item-deferred (item format)
  "Given a plist ITEM, add the FORMAT.

For use only in a `deferred:$' chain."
  (let ((item item)
        (format format)
        (d (deferred:new)))
    (request
      (format "%s/items" zotxt-url-base)
      :params `(("key" . ,(plist-get item :key))
                ("format" . ,(substring (symbol-name format) 1)))
      :parser (if (member format zotxt--json-formats)
                  #'zotxt--json-read
                #'buffer-string)
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (deferred:errorback-post d error-thrown)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (if (member format zotxt--json-formats)
                      ;; json data
                      (plist-put item format (elt data 0))
                    (plist-put item format data))
                  (deferred:callback-post d item))))
    d))

(defun zotxt-citekey-insert (arg)
  "Insert a citekey.

Prompts for search to choose item.  If prefix argument ARG is used,
will insert the currently selected item from Zotero.  If double
prefix argument is used the search method will have to be
selected even if `zotxt-default-search-method' is non-nil"
  (interactive "p")
  (let ((mk (point-marker)))
    (deferred:$
      (zotxt-choose-deferred arg)
      (deferred:nextc it
        (lambda (items)
          (zotxt-mapcar-deferred (lambda (item)
                                   (zotxt-get-item-deferred item :citekey))
                                 items)))
      (deferred:nextc it
        (lambda (items)
          (with-current-buffer (marker-buffer mk)
            (goto-char (marker-position mk))
            (insert (mapconcat
                     (lambda (item)
                       (format "@%s" (plist-get item :citekey)))
                     items " ")))))
      (deferred:error it #'zotxt--deferred-handle-error)
      (if zotxt--debug-sync (deferred:sync! it)))))

(define-obsolete-function-alias 'zotxt-easykey-insert #'zotxt-citekey-insert "6.0")

(defun zotxt-citekey-select-item-at-point ()
  "Select the item referred to by the citekey at point in Zotero."
  (interactive)
  (zotxt-select-citekey (zotxt-citekey-at-point)))

;;;###autoload
(define-minor-mode zotxt-citekey-mode
  "Toggle zotxt-citekey-mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

This is a minor mode for managing your citekey citations,
including completion."
  :init-value nil
  :lighter " ZotCitekey"
  :keymap zotxt-citekey-mode-map
  (if zotxt-citekey-mode
      (setq-local completion-at-point-functions
                  (cons 'zotxt-citekey-complete-at-point
                        completion-at-point-functions))
    (setq-local completion-at-point-functions
                (remove 'zotxt-citekey-complete-at-point
                        completion-at-point-functions))))

;;;###autoload
(define-obsolete-function-alias 'zotxt-easykey-mode #'zotxt-citekey-mode "6.0")

(provide 'zotxt)
;;; zotxt.el ends here
