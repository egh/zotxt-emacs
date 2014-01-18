(ert-deftest zotxt-test-url-retrieve ()
  (should (equal (zotxt-url-retrieve "http://localhost:23119/zotxt/items?key=0_ZBZQ4KMP&format=key") ["0_ZBZQ4KMP"])))

