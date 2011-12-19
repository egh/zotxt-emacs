var EXPORTED_SYMBOLS = ["instantiateCiteProc", "getItemId", "registerItemIds", "getCitationBlock", "getBibliographyData", "loadItems"];

var zotero = Components.classes["@zotero.org/Zotero;1"].getService().wrappedJSObject;

var mySys = {
    retrieveLocale : function (lang) {
        return zotero.Cite.System.retrieveLocale(lang);
    },
    retrieveItem : function(id) { 
        zotero.debug(zotero.localItems);
        if (zotero.localItems[id] != undefined) {
            return zotero.localItems[id];
        } else { 
            return zotero.Cite.System.retrieveItem(id);
        }
    }
};

function getCSL(style) {
    var locale = zotero.Prefs.get('export.bibliographyLocale');
    if(!locale) {
	var locale = zotero.locale;
	if(!locale) {
	    var locale = 'en-US';
	}
    }
    
    // determine version of parent style
    if(style.source) {
	var parentStyle = zotero.Styles.get(style.source);
	if(!parentStyle) {
	    throw(new Error('Style references '+style.source+', but this style is not installed',
			    zotero.Styles.ios.newFileURI(this.file).spec, null));
	}
	var version = parentStyle._version;
    } else {
	var version = this._version;
    }
    
    var xml = style.getXML();
    
    try {
	return new zotero.CiteProc.CSL.Engine(mySys, xml, locale);
    } catch(e) {
	zotero.logError(e);
	throw e;
    }
}

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
            zotero.reStructuredCSL = getCSL(style);
            zotero.localItems = {};
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

function loadItem(item) {
    zotero.debug(item['id']);
    zotero.localItems[item['id']] = item;
}

function loadItems(items) {
    zotero.debug(items);
    for (var id in items) {
        zotero.debug("LOADDDDDING");
        zotero.debug(items[id]);
        loadItem(items[id]);
    }
};

function isInTextStyle() {
    return ('in-text' === zotero.reStructuredCSL.opt.xclass);
};
