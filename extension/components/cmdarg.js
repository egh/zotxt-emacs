var Cc = Components.classes;
var Ci = Components.interfaces;

Components.utils.import("resource://gre/modules/XPCOMUtils.jsm");
Components.utils.import("resource://gre/modules/Services.jsm");

const nsIAppShellService    = Components.interfaces.nsIAppShellService;
const nsISupports           = Components.interfaces.nsISupports;
const nsICategoryManager    = Components.interfaces.nsICategoryManager;
const nsIComponentRegistrar = Components.interfaces.nsIComponentRegistrar;
const nsIObserver           = Components.interfaces.nsIObserver;
const nsICommandLine        = Components.interfaces.nsICommandLine;
const nsICommandLineHandler = Components.interfaces.nsICommandLineHandler;
const nsIFactory            = Components.interfaces.nsIFactory;
const nsIModule             = Components.interfaces.nsIModule;
const nsIWindowWatcher      = Components.interfaces.nsIWindowWatcher;

// chrome URI of your extension or application
const CHROME_URI = "chrome://jsbridge/content/";

// the contract id, CID, and category to be unique to your application.
const clh_contractID = "@mozilla.org/commandlinehandler/general-startup;1?type=jsbridge";

// use uuidgen to generate a unique ID
const clh_CID = Components.ID("{2872d428-14f6-11de-ac86-001f5bd9235c}");

// category names are sorted alphabetically. Typical command-line handlers use a
// category that begins with the letter "m".
const clh_category = "jsbridge";

/**
 * The XPCOM component that implements nsICommandLineHandler.
 * It is also an event observer to shutdown the (socket) server on shutdown.
 * It also implements nsIFactory to serve as its own singleton factory.
 */
function jsbridgeHandler() {
  this.port = 24242;
  this.server = null;
}
jsbridgeHandler.prototype = {
  classID: clh_CID,
  contractID: clh_contractID,
  classDescription: "jsbridgeHandler",
  _xpcom_categories: [{category: "profile-after-change", service: true},
                      {category: "command-line-handler", entry: clh_category}],

  QueryInterface : function clh_QI(iid)
  {
     if (iid.equals(nsIObserver) ||
         iid.equals(nsIFactory) ||
         iid.equals(nsISupports)||
         iid.equals(nsICommandLineHandler))
       return this;
     throw Components.results.NS_ERROR_NO_INTERFACE;
   },

  /* nsIObserver */

  observe : function(aSubject, aTopic, aData) {
        switch(aTopic) {
        case "profile-after-change":
            this.init();
            break;
            
        case "quit-application":
            this.uninit();
            break;
        }
    },

  /* nsICommandLineHandler */

  handle : function clh_handle(cmdLine)
  {
    var port = cmdLine.handleFlagWithParam("jsbridge", false);
    this.port = parseInt(port) || this.port;
    this.startServer();
  },

  // follow the guidelines in nsICommandLineHandler.idl
  // specifically, flag descriptions should start at
  // character 24, and lines should be wrapped at
  // 72 characters with embedded newlines,
  // and finally, the string should end with a newline
  helpInfo : "  -jsbridge            Port to run jsbridge on.\n",

  /* nsIFactory */

  createInstance : function clh_CI(outer, iid)
  {
    if (outer != null)
      throw Components.results.NS_ERROR_NO_AGGREGATION;

    return this.QueryInterface(iid);
  },

  lockFactory : function clh_lock(lock)
  {
    /* no-op */
  },

  /* internal methods */

  startServer: function() {
        server = {};
        // import the server
        try {
            // use NSPR sockets to get offline+localhost support - needs recent js-ctypes
            Components.utils.import('resource://jsbridge/modules/nspr-server.js', server);
        }
        catch(e) {
            dump("jsbridge can't use NSPR sockets, falling back to nsIServerSocket - " +
                 "OFFLINE TESTS WILL FAIL\n");
            Components.utils.import('resource://jsbridge/modules/server.js', server);
        }
        
        // start the server
        this.server = server.startServer(this.port);
  },
  
  init: function() {
        Services.obs.addObserver(this, "quit-application", false);
    },

  uninit: function() {
    Services.obs.removeObserver(this, "quit-application", false);
    this.server.stop();
    this.server = null;
  }
};

/**
 * XPCOMUtils.generateNSGetFactory was introduced in Mozilla 2 (Firefox 4).
 * XPCOMUtils.generateNSGetModule is for Mozilla 1.9.1 (Firefox 3.5).
 */
if (XPCOMUtils.generateNSGetFactory)
  const NSGetFactory = XPCOMUtils.generateNSGetFactory([jsbridgeHandler]);
else
  const NSGetModule = XPCOMUtils.generateNSGetModule([jsbridgeHandler]);
