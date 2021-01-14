;; -*- lexical-binding: t -*-

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
                  (let ((n n)
                        (d (deferred:new)))
                    (deferred:setTimeout
                      (lambda ()
                        (deferred:callback-post d (+ 1 n))) 1)
                    d))
                  lst)))
           (deferred:sync! it))))
    (should (equal '(2 3 4) results))))

(ert-deftest zotxt-test-deferred-error-handler ()
  (let ((err (should-error
              (deferred:$
                (deferred:next
                  (lambda ()
                    (error "Huh")))
                (deferred:error it #'zotxt--deferred-handle-error)
                (deferred:sync! it)))))
    (should (equal
             (cdr err)
             "Huh"))))

(ert-deftest zotxt-test-get-item-deferred ()
  (should (equal
           (let ((item
                  (deferred:$
                    (deferred:next
                      (lambda ()
                        '(:key "0_ZBZQ4KMP")))
                    (deferred:nextc it
                      (lambda (item)
                        (zotxt-get-item-deferred item :citekey)))
                    (deferred:sync! it))))
             (plist-get item :citekey))
           "doe:2005first")))

(ert-deftest zotxt-test-get-item-uuid-deferred ()
  (should (equal
           (let ((item
                  (deferred:$
                    (deferred:next
                      (lambda ()
                        '(:key "0_ZBZQ4KMP")))
                    (deferred:nextc it
                      (lambda (item)
                        (zotxt-get-item-deferred item :248bebf1-46ab-4067-9f93-ec3d2960d0cd)))
                    (deferred:sync! it))))
             (plist-get item :248bebf1-46ab-4067-9f93-ec3d2960d0cd))
           "{ | Doe, 2005 | | |zu:1254:ZBZQ4KMP}")))

(ert-deftest zotxt-test-get-item-citekey-list-chain ()
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
                           (zotxt-get-item-deferred item :citekey))
                         items)))
                    (deferred:sync! it)))
                  (citekeys (mapcar (lambda (item)
                                      (plist-get item :citekey)) items)))
             (sort citekeys #'string-lessp))
           '("doe:2005first" "doe:2007why"))))

(ert-deftest zotxt-test-get-item-bibliography-deferred ()
  (let ((text "Doe, John. First Book. Cambridge: Cambridge University Press, 2005.")
        (html "<div style=\"line-height: 1.35; padding-left: 2em; text-indent:-2em;\" class=\"csl-bib-body\">
  <div class=\"csl-entry\">Doe, John. <i>First Book</i>. Cambridge: Cambridge University Press, 2005.</div>
  <span class=\"Z3988\" title=\"url_ver=Z39.88-2004&amp;ctx_ver=Z39.88-2004&amp;rfr_id=info%3Asid%2Fzotero.org%3A2&amp;rft_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Abook&amp;rft.genre=book&amp;rft.btitle=First%20Book&amp;rft.place=Cambridge&amp;rft.publisher=Cambridge%20University%20Press&amp;rft.aufirst=John&amp;rft.aulast=Doe&amp;rft.au=John%20Doe&amp;rft.date=2005\"></span>
</div>")
        (item
         (deferred:$
           (deferred:next (lambda () '(:key "0_ZBZQ4KMP")))
           (deferred:nextc it
             (lambda (item) (zotxt-get-item-bibliography-deferred item)))
           (deferred:sync! it))))
    (should (equal text (plist-get item :citation)))
    (should (equal html (plist-get item :citation-html)))
    (should (equal text (plist-get item :chicago-note-bibliography)))
    (should (equal html (plist-get item :chicago-note-bibliography-html)))))

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

(ert-deftest org-zotxt-test-get-item-link-text-deferred-with-citekey ()
  (let ((text "doe:2005first")
        (item
         (deferred:$
           (deferred:next (lambda () '(:key "0_ZBZQ4KMP")))
           (deferred:nextc it
             (lambda (item)
               (let ((org-zotxt-link-description-style :citekey))
                 (org-zotxt-get-item-link-text-deferred item))))
            (deferred:sync! it))))
    (should (equal text (plist-get item :citekey)))))

(ert-deftest org-zotxt-test-extract-link-id-at-point ()
  (with-temp-buffer
    (org-mode)
    (org-zotxt-mode)
    (insert "[[zotero://select/items/0_ZBZQ4KMP][Doe, John. First Book. Cambridge: Cambridge University Press, 2005.]]")
    (goto-char (point-min))
    (should (equal (org-zotxt-extract-link-id-at-point) "0_ZBZQ4KMP"))))

(ert-deftest org-zotxt-extract-link-id-from-path ()
  (let ((link-id "0_ZBZQ4KMP"))
    (should (equal link-id
		   (org-zotxt-extract-link-id-from-path "zotero://select/items/0_ZBZQ4KMP")))
    (should (equal link-id
		   (org-zotxt-extract-link-id-from-path "//select/items/0_ZBZQ4KMP")))))

(ert-deftest org-zotxt-test-update-reference-link-at-point ()
  (let ((org-zotxt-link-description-style :citation)
        (start-text "[[zotero://select/items/0_ZBZQ4KMP][foo]]")
        (end-text "[[zotero://select/items/0_ZBZQ4KMP][Doe, John. First Book. Cambridge: Cambridge University Press, 2005.]]"))
    (with-temp-buffer
      (org-mode)
      (org-zotxt-mode)
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
      (org-mode)
      (org-zotxt-mode)
      (insert start-text)
      (goto-char (point-min))
      (org-zotxt-update-all-reference-links)
      (while (> 250 (length (buffer-string)))
        (sleep-for 0 1))
      (should (equal (buffer-string) end-text)))))

(ert-deftest zotxt-test-search-deferred ()
  (let ((results
         (deferred:$
           (zotxt-search-deferred :title-creator-year "doe first book")
           (deferred:sync! it))))
    (should (equal results '((:key "0_ZBZQ4KMP"))))))

(ert-deftest org-zotxt-test-insert-reference-link-to-item ()
  (let ((org-zotxt-link-description-style :citation)
        (text "[[zotero://select/items/foo][Foo Bar]]")
        (item '(:key "foo" :citation "Foo Bar")))
    (with-temp-buffer
      (org-mode)
      (org-zotxt-mode)
      (org-zotxt-insert-reference-link-to-item item)
      (should (equal (buffer-string) text)))))

(ert-deftest org-zotxt-test-insert-reference-link-to-item-with-citekey ()
  (let ((org-zotxt-link-description-style :citekey)
        (text "[[zotero://select/items/foo][@foo:2014bar]]")
        (item '(:key "foo" :citekey "foo:2014bar")))
    (with-temp-buffer
      (org-mode)
      (org-zotxt-mode)
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
      (org-mode)
      (org-zotxt-mode)
      (org-zotxt-insert-reference-links-to-items items)
      (should (equal (buffer-string) text)))))

(ert-deftest org-zotxt-make-quick-bib-string ()
  (let ((item
         (deferred:$
           (deferred:next (lambda () '(:key "0_ZBZQ4KMP")))
           (deferred:nextc it
             (lambda (item) (zotxt-get-item-deferred item :json)))
           (deferred:sync! it))))
    (should (equal "Doe, John - First Book"
                   (zotxt-make-quick-bib-string item)))))
