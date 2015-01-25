(require 'f)

(defvar zotxt-support-path
  (f-dirname load-file-name))

(defvar zotxt-features-path
  (f-parent zotxt-support-path))

(defvar zotxt-root-path
  (f-parent zotxt-features-path))

(add-to-list 'load-path zotxt-root-path)

(require 'zotxt)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
