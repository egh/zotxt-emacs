(ert-deftest zotxt-test-url-retrieve ()
  (should (equal (zotxt-url-retrieve "http://127.0.0.1:23119/zotxt/items?key=0_ZBZQ4KMP&format=key") ["0_ZBZQ4KMP"])))

(ert-deftest zotxt-test-easykey-get-item-easykey ()
  (should (equal (zotxt-easykey-get-item-easykey "0_ZBZQ4KMP") "doe:2005first")))
