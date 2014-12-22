(require 'f)

(defvar zotxt-root-path
  (f-join (f-parent (f-dirname load-file-name)) "elisp"))

(add-to-list 'load-path zotxt-root-path)

(require 'zotxt)
(require 'org-zotxt)

