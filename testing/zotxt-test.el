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
                        (zotxt-mapcar-deferred items #'zotxt-get-item-easykey-deferred)))
                    (deferred:sync! it)))
                  (easykeys (mapcar (lambda (item)
                                      (plist-get item :easykey)) items)))
             (sort easykeys #'string-lessp))
           '("doe:2005first" "doe:2007why"))))
