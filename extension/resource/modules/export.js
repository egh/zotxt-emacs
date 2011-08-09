
var EXPORTED_SYMBOLS = ["instantiateCiteproc", "getItemId", "registerItemIds", "getCitationBlock", "getBibliographyData"];

var zotero = Components.classes["@zotero.org/Zotero;1"].getService().wrappedJSObject;

function instantiateCiteproc (style_name) {
	return zotero.CiteProc.CSL;
};

function getItemId (key) {
};

function registerItemIds (ids) {
};

function getCitationBlock (citation) {
};

function getBibliographyData () {
};
