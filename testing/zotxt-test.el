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
         
(ert-deftest zotxt-test-get-item-easykey ()
  (should (equal
           (plist-get (zotxt-get-item-easykey '(:key "0_ZBZQ4KMP")) :easykey)
           "doe:2005first")))

(ert-deftest zotxt-test-get-item-easykey-deferred ()
  (should (equal
           (let ((item
                  (deferred:$
                    (deferred:next
                      (lambda ()
                        '(:key "0_ZBZQ4KMP")))
                    (deferred:nextc it
                      (lambda (item)
                        (zotxt-get-item-easykey-deferred item)))
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
                         #'zotxt-get-item-easykey-deferred
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
    (should (equal text (plist-get item :bibliography)))
    (should (equal text (plist-get item :chicago-note-bibliography)))))

(ert-deftest org-zotxt-test-update-reference-link-at-point ()
  (let ((start-text "[[zotero://select/items/0_ZBZQ4KMP][foo]]")
        (end-text "[[zotero://select/items/0_ZBZQ4KMP][Doe, John. First Book. Cambridge: Cambridge University Press, 2005.]]"))
    (with-temp-buffer
      (insert start-text)
      (goto-char (point-min))
      (org-zotxt-update-reference-link-at-point)
      (while (string= start-text (buffer-string))
        (sleep-for 0 1))
      (should (equal (buffer-string) end-text)))))
                   

