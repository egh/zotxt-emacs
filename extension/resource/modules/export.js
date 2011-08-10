
var EXPORTED_SYMBOLS = ["instantiateCiteproc", "getItemId", "registerItemIds", "getCitationBlock", "getBibliographyData"];

var zotero = Components.classes["@zotero.org/Zotero;1"].getService().wrappedJSObject;

function instantiateCiteproc (style_name) {
	// Note to self: need to really instantiate this.
	return zotero.CiteProc.CSL;
};


function getItemId (idStr) {
    if (!idStr.match(/^[0-9]+_/)) {
        idStr = "0_" + idStr;
	}
	var lkh = zotero.Items.parseLibraryKeyHash(idStr);
    var item = zotero.Items.getByLibraryAndKey(lkh.libraryID, lkh.key);
	return item.id;
};

function registerItemIds (ids) {
};

function getCitationBlock (citation) {
};

function getBibliographyData () {
};
