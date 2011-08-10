
var EXPORTED_SYMBOLS = ["instantiateCiteProc", "getItemId", "registerItemIds", "getCitationBlock", "getBibliographyData"];

var zotero = Components.classes["@zotero.org/Zotero;1"].getService().wrappedJSObject;

function getItemId (idStr) {
    if (!idStr.match(/^[0-9]+_/)) {
        idStr = "0_" + idStr;
	}
	var lkh = zotero.Items.parseLibraryKeyHash(idStr);
    var item = zotero.Items.getByLibraryAndKey(lkh.libraryID, lkh.key);
	return item.id;
};

/*
* Locale will be the Zotero export locale.
*/
function instantiateCiteProc (styleid) {
	// Suspenders and a belt.
	try {
		if (!styleid) {
			styleid = "chicago-author-date";
		}
		if (styleid.slice(0,7) !== 'http://') {
			styleid = 'http://www.zotero.org/styles/' + styleid;
		}
		zotero.debug("XXX does this exist?: " + styleid);
		var style = zotero.Styles.get(styleid);
		zotero.reStructuredCSL = style.csl;
		zotero.reStructuredCSL.setOutputFormat("html");
	} catch (e) {
		zotero.debug("XXX instantiateCiteProc oops: " + e);
	}
};


function registerItemIds (ids) {
	zotero.reStructuredCSL.updateItems(ids);
};

function getCitationBlock (citation) {
	zotero.debug("XXX citation: " + citation.citationItems + "\n");
	try {
		ret = zotero.reStructuredCSL.appendCitationCluster(citation);
	} catch (e) {
		zotero.debug("XXX  oops: "+e);
	}
	zotero.debug("XXX   ret: " + ret);
	zotero.debug("XXX   ret: " + ret.length);
	return ret[0][1];
};

function getBibliographyData () {
};
