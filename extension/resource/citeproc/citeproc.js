var EXPORTED_SYMBOLS = ["instantiateCiteProc", "getItemIdBatch", "updateItems", "appendCitationClusterBatch", "makeBibliography", "registerLocalItems"];

var zotero = Components.classes["@zotero.org/Zotero;1"].getService().wrappedJSObject;

/**
 * Proxy sys item for passing in to citeproc. Wraps the
 * zotero.Cite.System object, but allows for locally registered items.
 */
var mySys = {
    retrieveLocale : function (lang) {
        return zotero.Cite.System.retrieveLocale(lang);
    },
    retrieveItem : function(id) { 
        if (zotero.localItems[id] != undefined) {
            return zotero.localItems[id];
        } else { 
            return zotero.Cite.System.retrieveItem(id);
        }
    }
};

/**
 * Escape strings in a javascript object for safe pass-through (7 bit
 * only).
 */
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

/**
 * Encode a string for passing back to Python.
 */
function encode(what) {
    return JSON.stringify(escapeStringValues(what));
}

/**
 * Return an array of item IDs for an array of keys.
 */
function getItemIdBatch (keys) {
    return encode(keys.map(getItemId));
}

/**
 * Get the item ID for a particular key.
 */
function getItemId (keyStr) {
    var libraryId = null;
    var key = null;
    if (!keyStr.match(/^[0-9]+_/)) {
        keyStr = "0_" + keyStr;
    }
    var md = keyStr.match(/^0_(.*)$/);
    if (md) {
        /* avoid looking things up, local library */
        key = md[1];
    } else {
	var lkh = zotero.Items.parseLibraryKeyHash(keyStr);
        libraryId = lkh.libraryId;
        key = lkh.key;
    }
    var item = zotero.Items.getByLibraryAndKey(libraryId, key);
    return item.id;
};

/**
 * Get a new citeproc object.
 */
function instantiateCiteProc (styleid) {
    if (!styleid.match(/^http:/)) {
	styleid = 'http://www.zotero.org/styles/' + styleid;
    }
    var style = zotero.Styles.get(styleid);
    /* TODO Allow passing in locale? **/
    var locale = zotero.Prefs.get('export.bibliographyLocale');
    if(!locale) {
	locale = zotero.locale;
	if(!locale) {
	    locale = 'en-US';
	}
    }
    
    try {
	zotero.reStructuredCSL = new zotero.CiteProc.CSL.Engine(mySys, style.getXML(), locale);
    } catch(e) {
	zotero.logError(e);
	throw e;
    }
    zotero.localItems = {};
    zotero.reStructuredCSL.setOutputFormat("html");
};


/**
 * Wrapper for citeproc updateItems.
 */
function updateItems (ids) {
    zotero.reStructuredCSL.updateItems(ids);
};

/**
 * Batch appendCitationCluster.
 */
function appendCitationClusterBatch (citations) {
    return encode(citations.map(appendCitationCluster));
}

/**
 * Wrapper for appendCitationCluster.
 */
function appendCitationCluster (citation) {
    var results;
    results = zotero.reStructuredCSL.appendCitationCluster(citation, true);
    var index = citation['properties']['index'];
    for (var i = 0 ; i <= results.length ; i++) {
        if (results[i][0] == index) {
            return "" + results[i][1];
        }
    }
    return null;
};

/**
 * Wrapper for citeproc makeBibliography.
 */
function makeBibliography (arg) {
    var bib = zotero.reStructuredCSL.makeBibliography(arg);
    if (bib) {
        return encode(bib);
    } else {
        return "";
    }
};

/**
 * Register local items. Should be an object.
 */
function registerLocalItems(items) {
    for (var id in items) {
        var item = items[id];
        zotero.localItems[item['id']] = item;
    }
};

/**
 * Return true if this is an "in text" style, false if it is a
 * footnote style.
 */
function isInTextStyle() {
    return ('in-text' === zotero.reStructuredCSL.opt.xclass);
};
