(require 'moz)

(defvar zotero-default-bibliography-style
  "http://www.zotero.org/styles/chicago-note-bibliography"
  "Default bibliography style to use.")

(defun zotero-clean-bib-entry (entry)
  "Clean up a bibliography entry as returned by Zotero."
  (let ((retval entry))
    (setq retval (replace-regexp-in-string "\n" "" retval))
    (setq retval (replace-regexp-in-string "\" "“" retval))
    (setq retval (replace-regexp-in-string "\" "’" retval))
    (setq retval (replace-regexp-in-string "\^]" "”" retval))
    retval))

(defun zotero-reset-mozrepl ()
  (delete-process (inferior-moz-process)))

(defun zotero-send-string (str)
  (comint-send-string (inferior-moz-process) str))

(defun zotero-send-strings (str-list)
  (mapc 'zotero-send-string str-list))

(defun zotero-js-write-to-file (filename expr)
  (let ((js-func-def "
function writeFile(filename, data) {
  var file = Components.classes[\"@mozilla.org/file/local;1\"].createInstance(Components.interfaces.nsILocalFile);
  var foStream = Components.classes[\"@mozilla.org/network/file-output-stream;1\"].createInstance(Components.interfaces.nsIFileOutputStream);
  file.initWithPath(filename); 
  foStream.init(file, 0x02 | 0x08, 00666, 0);
  foStream.write(data, data.length);
  foStream.close();
};"))
    (zotero-reset-mozrepl)
    (zotero-send-strings 
     (list js-func-def
           (format "writeFile(\"%s\", %s);" filename expr)))))

(defun zotero-js-get-string-value (expr)
  (let ((tmp-file (make-temp-file "zotero")))
    (delete-file tmp-file) ;; delete our version
    (zotero-js-write-to-file tmp-file expr)
    (while (not (file-exists-p tmp-file))
      (sleep-for 0 10))
    (prog1 (save-excursion
             (with-temp-buffer
               (insert-file-contents tmp-file)
               (buffer-substring-no-properties (point-min) (point-max))))
      (delete-file tmp-file))))
  
(defun zotero-generate-bib-entry-from-id (item-id &optional style bib-format)
  (let* ((bib-type (format "bibliography=%s"
                           (or style zotero-default-bibliography-style)))
         (bib-format (or bib-format "text")))
    (if (not (string-match "^[0-9]+_" item-id))
        (setq item-id (format "0_%s" item-id)))
    (zotero-reset-mozrepl)
    (zotero-send-strings
     `("var Zotero = Components.classes[\"@zotero.org/Zotero;1\"] .getService(Components.interfaces.nsISupports).wrappedJSObject;"
       ,(format "var lkh = Zotero.Items.parseLibraryKeyHash(\"%s\");" item-id)
       "var item = Zotero.Items.getByLibraryAndKey(lkh.libraryID, lkh.key);"
       ,(format 
          "var biblio = Zotero.QuickCopy.getContentFromItems(new Array(item), \"%s\");" 
          bib-type)))
    (zotero-clean-bib-entry
     (zotero-js-get-string-value (format "biblio.%s" bib-format)))))

(defun zotero-get-selected-item-ids ()
  (zotero-reset-mozrepl)
  (zotero-send-strings
   `("var ZoteroPane = Components.classes[\"@mozilla.org/appshell/window-mediator;1\"] .getService(Components.interfaces.nsIWindowMediator).getMostRecentWindow(\"navigator:browser\").ZoteroPane;"
     "var selected_items = ZoteroPane.getSelectedItems();"))
  (let ((item-count (string-to-int
                     (zotero-js-get-string-value "new String(selected_items.length)")))
        (retval nil))
    (dotimes (i item-count)
      (let ((library-id (zotero-js-get-string-value 
                         (format "selected_items[%s].libraryID" i)))
            (item-key (zotero-js-get-string-value 
                       (format "selected_items[%s].key" i))))
        (if (string= library-id "")
            (setq library-id "0"))
      (setq retval (cons (format "%s_%s" library-id item-key) retval))))
    retval))

(provide 'zotero)
