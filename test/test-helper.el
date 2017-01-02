(add-to-list 'load-path (f-parent (f-dirname load-file-name)))


(require 'undercover)
(undercover "*.el")

(require 'zotxt)
(require 'org-zotxt)

(setq zotxt-url-base "http://127.0.0.1:33119/zotxt")
