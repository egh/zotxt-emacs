(require 'f)

(defvar zotxt-support-path
  (f-dirname load-file-name))

(defvar zotxt-features-path
  (f-parent zotxt-support-path))

(defvar zotxt-root-path
  (f-parent zotxt-features-path))

(add-to-list 'load-path zotxt-root-path)

(require 'org)
(require 'zotxt)
(require 'org-zotxt)
(require 'espuds)
(require 'ert)

(Setup
 (setq zotxt-url-base "http://127.0.0.1:33119/zotxt"
       zotxt--debug-sync t))

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
