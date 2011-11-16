var EXPORTED_SYMBOLS = ["instantiateCiteProc", "getItemId", "registerItemIds", "getCitationBlock", "getBibliographyData"];

var zotero = Components.classes["@zotero.org/Zotero;1"].getService().wrappedJSObject;

function getItemId (idStr) {
    var libraryId = null;
    var key = null;
    if (!idStr.match(/^[0-9]+_/)) {
        idStr = "0_" + idStr;
    }
    var md = idStr.match(/^0_(.*)$/);
    if (md) {
        /* avoid looking things up, local library */
        key = md[1];
    } else {
	var lkh = zotero.Items.parseLibraryKeyHash(idStr);
        libraryId = lkh.libraryId;
        key = lkh.key;
    }
    var item = zotero.Items.getByLibraryAndKey(libraryId, key);
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
    var results;
    try {
	results = zotero.reStructuredCSL.appendCitationCluster(citation, true);
    } catch (e) {
	zotero.debug("XXX  oops: "+e);
    }
    var index = citation['properties']['index'];
    for (var i = 0 ; i <= results.length ; i++) {
        if (results[i][0] == index) {
            return escape("" + results[i][1]);
        }
    }
    return "";
};

function escapeStringValues (o) {
    if (Object.prototype.toString.call(o) === '[object Array]') {
        return o.map(function (x) { return escapeStringValues(x); });
    } else if (typeof o === "string") {
        return escape(o);
    } else if (typeof o === "object") {
        var retval = new Object();
        for (var k in o) {
            retval[k] = escapeStringValues(o[k]);
        }
        return retval;
    } else {
        return o;
    }
};

function getBibliographyData (arg) {
	var ret;
	zotero.debug("XXX WTF?");
	try {
		zotero.debug("XXX WTF? part two");
		ret = zotero.reStructuredCSL.makeBibliography(arg);
		zotero.debug("XXX WTF? part three");
		if (ret) {
			zotero.debug("XXX WTF? part four");
                	ret = escapeStringValues(ret);
			ret = JSON.stringify(ret);
			zotero.debug("XXX WTF? part five");
		}
	} catch (e) {
		zotero.debug("XXX oops: "+e);
	}
	zotero.debug("XXX non-oops: "+ret);
	return ret;
};

function isInTextStyle() {
	return ('in-text' === zotero.reStructuredCSL.opt.xclass);
};
