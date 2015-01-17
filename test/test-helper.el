(require 'f)

(defvar zotxt-root-path
  (f-parent (f-dirname load-file-name)))

(add-to-list 'load-path zotxt-root-path)

(require 'zotxt)
(require 'org-zotxt)

