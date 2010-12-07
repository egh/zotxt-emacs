{
    "translatorID":"a9e42a90-fdc9-450b-ab1d-048f5cb6cf7a",
    "translatorType":2,
    "label":"org-mode",
    "creator":"Erik Hetzner",
    "target":"html",
    "minVersion":"1.0.0b4.r1",
    "maxVersion":"",
    "priority":200,
    "inRepository":false,
    "lastUpdated":"2010-11-27 18:50:15",
}

Zotero.configure("dataMode", "block");
Zotero.addOption("exportCharset", "UTF-8");

function doExport() {
    var citekeys = new Object();
    var item;
    while(item = Zotero.nextItem()) {
        var library_id = item.LibraryID ? item.LibraryID : 0;
        Zotero.write("[[zotero://select//" + library_id + "_" + item.key + "][" + library_id + "_" + item.key + "]]");
        Zotero.write("\n");
    }
}
