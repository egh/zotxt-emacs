(setq load-dir (f-parent (f-dirname load-file-name)))
(setq base-dir (f-parent (f-parent load-dir)))
(add-to-list 'load-path base-dir)


(require 'undercover)
(undercover "*.el")

(require 'zotxt)
(require 'org-zotxt)

(async-shell-command
 (format "bundle exec ruby %s/mock-server.rb -p 33119" base-dir))
(sleep-for 2)
(setq zotxt-url-base "http://127.0.0.1:33119/zotxt")
