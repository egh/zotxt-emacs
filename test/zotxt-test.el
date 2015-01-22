(ert-deftest zotxt-test-mapcar-deferred ()
  (let ((results
         (deferred:$
           (deferred:next
             (lambda ()
               '(1 2 3)))
           (deferred:nextc it
             (lambda (lst)
               (zotxt-mapcar-deferred
                (lambda (n)
                  (lexical-let ((n n)
                                (d (deferred:new)))
                    (deferred:setTimeout
                      (lambda ()
                        (deferred:callback-post d (+ 1 n))) 1)
                    d))
                  lst)))
           (deferred:sync! it))))
    (should (equal '(2 3 4) results))))
         
(ert-deftest zotxt-test-get-item-deferred ()
  (should (equal
           (let ((item
                  (deferred:$
                    (deferred:next
                      (lambda ()
                        '(:key "0_ZBZQ4KMP")))
                    (deferred:nextc it
                      (lambda (item)
                        (zotxt-get-item-deferred item :easykey)))
                    (deferred:sync! it))))
             (plist-get item :easykey))
           "doe:2005first")))

(ert-deftest zotxt-test-get-item-easykey-list-chain ()
  (should (equal
           (let* ((items
                  (deferred:$
                    (deferred:next
                      (lambda ()
                        '((:key "0_ZBZQ4KMP")
                          (:key "0_TWCW4IJ7"))))
                    (deferred:nextc it
                      (lambda (items)
                        (zotxt-mapcar-deferred
                         (lambda (item)
                           (zotxt-get-item-deferred item :easykey))
                         items)))
                    (deferred:sync! it)))
                  (easykeys (mapcar (lambda (item)
                                      (plist-get item :easykey)) items)))
             (sort easykeys #'string-lessp))
           '("doe:2005first" "doe:2007why"))))

(ert-deftest zotxt-test-get-item-bibliography-deferred ()
  (let ((text "Doe, John. First Book. Cambridge: Cambridge University Press, 2005.")
        (item
         (deferred:$
           (deferred:next (lambda () '(:key "0_ZBZQ4KMP")))
           (deferred:nextc it
             (lambda (item) (zotxt-get-item-bibliography-deferred item)))
           (deferred:sync! it))))
    (should (equal text (plist-get item :citation)))
    (should (equal text (plist-get item :chicago-note-bibliography)))))

(ert-deftest org-zotxt-test-get-item-link-text-deferred ()
  (let ((text "Doe, John. First Book. Cambridge: Cambridge University Press, 2005.")
        (item
         (deferred:$
           (deferred:next (lambda () '(:key "0_ZBZQ4KMP")))
           (deferred:nextc it
             (lambda (item)
               (let ((org-zotxt-link-description-style :citation))
                 (org-zotxt-get-item-link-text-deferred item))))
           (deferred:sync! it))))
    (should (equal text (plist-get item :citation)))))

(ert-deftest org-zotxt-test-get-item-link-text-deferred-with-easykey ()
  (let ((text "doe:2005first")
        (item
         (deferred:$
           (deferred:next (lambda () '(:key "0_ZBZQ4KMP")))
           (deferred:nextc it
             (lambda (item)
               (let ((org-zotxt-link-description-style :easykey))
                 (org-zotxt-get-item-link-text-deferred item))))
            (deferred:sync! it))))
    (should (equal text (plist-get item :easykey)))))

(ert-deftest org-zotxt-test-extract-link-id-at-point ()
  (with-temp-buffer
    (insert "[[zotero://select/items/0_ZBZQ4KMP][Doe, John. First Book. Cambridge: Cambridge University Press, 2005.]]")
    (goto-char (point-min))
    (should (equal (org-zotxt-extract-link-id-at-point) "0_ZBZQ4KMP"))))

(ert-deftest org-zotxt-test-update-reference-link-at-point ()
  (let ((org-zotxt-link-description-style :citation)
        (start-text "[[zotero://select/items/0_ZBZQ4KMP][foo]]")
        (end-text "[[zotero://select/items/0_ZBZQ4KMP][Doe, John. First Book. Cambridge: Cambridge University Press, 2005.]]"))
    (with-temp-buffer
      (insert start-text)
      (goto-char (point-min))
      (org-zotxt-update-reference-link-at-point)
      (while (string= start-text (buffer-string))
        (sleep-for 0 1))
      (should (equal (buffer-string) end-text)))))

(ert-deftest org-zotxt-test-update-all-reference-links ()
  (let ((org-zotxt-link-description-style :citation)
        (start-text "[[zotero://select/items/0_ZBZQ4KMP][foo]]
[[zotero://select/items/0_TWCW4IJ7][bar]]")
        (end-text "[[zotero://select/items/0_ZBZQ4KMP][Doe, John. First Book. Cambridge: Cambridge University Press, 2005.]]
[[zotero://select/items/0_TWCW4IJ7][Doe, John, and Jenny Roe. “Why Water Is Wet.” In Third Book, edited by Sam Smith. Oxford: Oxford University Press, 2007.]]"))
    (with-temp-buffer
      (insert start-text)
      (goto-char (point-min))
      (org-zotxt-update-all-reference-links)
      (while (> 200 (length (buffer-string)))
        (sleep-for 0 1))
      (should (equal (buffer-string) end-text)))))

(ert-deftest zotxt-test-choose-deferred ()
  (let ((results
         (deferred:$
           (zotxt-choose-deferred :title-creator-year "doe first book")
           (deferred:sync! it))))
    (should (equal results '((:key "0_ZBZQ4KMP" :citation "Doe, John. First Book. Cambridge: Cambridge University Press, 2005."))))))

(ert-deftest org-zotxt-test-insert-reference-link-to-item ()
  (let ((org-zotxt-link-description-style :citation)
        (text "[[zotero://select/items/foo][Foo Bar]]")
        (item '(:key "foo" :citation "Foo Bar")))
    (with-temp-buffer
      (org-zotxt-insert-reference-link-to-item item)
      (should (equal (buffer-string) text)))))

(ert-deftest org-zotxt-test-insert-reference-link-to-item-with-easykey ()
  (let ((org-zotxt-link-description-style :easykey)
        (text "[[zotero://select/items/foo][@foo:2014bar]]")
        (item '(:key "foo" :easykey "foo:2014bar")))
    (with-temp-buffer
      (org-zotxt-insert-reference-link-to-item item)
      (should (equal (buffer-string) text)))))

(ert-deftest org-zotxt-test-insert-reference-link-to-item-with-betterbibtexkey ()
  (let ((org-zotxt-link-description-style :betterbibtexkey)
        (text "[[zotero://select/items/foo][@foo:2014bar]]")
        (item '(:key "foo" :betterbibtexkey "foo:2014bar")))
    (with-temp-buffer
      (org-zotxt-insert-reference-link-to-item item)
      (should (equal (buffer-string) text)))))

(ert-deftest org-zotxt-test-insert-reference-links-to-items ()
  (let ((org-zotxt-link-description-style :citation)
        (text "[[zotero://select/items/foo][Foo Bar]]
[[zotero://select/items/bar][Bar Foo]]
")
        (items '((:key "foo" :citation "Foo Bar")
                 (:key "bar" :citation "Bar Foo"))))
    (with-temp-buffer
      (org-zotxt-insert-reference-links-to-items items)
      (should (equal (buffer-string) text)))))
