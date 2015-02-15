(require 'f)

(defvar zotxt-root-path
  (f-parent (f-dirname load-file-name)))

(add-to-list 'load-path zotxt-root-path)

(require 'undercover)
(undercover "*.el")

(require 'zotxt)
(require 'org-zotxt)

(setq zotxt-url-base "http://127.0.0.1:33119/zotxt")
