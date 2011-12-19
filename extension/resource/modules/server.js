/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Mozilla Corporation Code.
 *
 * The Initial Developer of the Original Code is
 * MozRepl project.
 * Portions created by the Initial Developer are Copyright (C) 2008
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Mikeal Rogers <mikeal.rogers@gmail.com>
 * Massimiliano Mirra <bard@hyperstruct.net>
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

var EXPORTED_SYMBOLS = ["Server", "AsyncRead", "Session", "sessions", "startServer"];

const DEBUG_ON = true;
const DO_FILE_LOGGING = true;
const BUFFER_SIZE = 1024;
var Cc = Components.classes;
var Ci = Components.interfaces;
var bridge = {}; Components.utils.import("resource://jsbridge/modules/bridge.js", bridge);
var gJsbridgeFileLogger = {}; Components.utils.import("resource://jsbridge/modules/jsbridgefilelogger.js", gJsbridgeFileLogger);

var hwindow = Components.classes["@mozilla.org/appshell/appShellService;1"]
    .getService(Components.interfaces.nsIAppShellService)
    .hiddenDOMWindow;

var uuidgen = Components.classes["@mozilla.org/uuid-generator;1"]
    .getService(Components.interfaces.nsIUUIDGenerator);

function AsyncRead (session) {
  this.session = session;
}
AsyncRead.prototype.onStartRequest = function (request, context) {};
AsyncRead.prototype.onStopRequest = function (request, context, status) {
  this.session.onQuit();
}
AsyncRead.prototype.onDataAvailable = function (request, context, inputStream, offset, count) {
  var str = {};
  str.value = '';
  var bytesAvail = 0;
  do {
    var parts = {};
    if (count > BUFFER_SIZE) {
      bytesAvail = BUFFER_SIZE;
    } else {
      bytesAvail = count;
    }
    var bytesRead = this.session.instream.readString(bytesAvail, parts);
    count = count - bytesRead;
    str.value += parts.value;
  } while (count > 0);
  this.session.receive(str.value);
}

backstage = this;

function Session (transport) {
  this.transpart = transport;
  this.sandbox = Components.utils.Sandbox(backstage);
  this.sandbox.bridge = new bridge.Bridge(this);
  this.sandbox.openPreferences = hwindow.openPreferences;
  try {
      this.outputstream = transport.openOutputStream(Ci.nsITransport.OPEN_BLOCKING, 0, 0);	
      this.outstream = Cc['@mozilla.org/intl/converter-output-stream;1']
                    .createInstance(Ci.nsIConverterOutputStream);
      this.outstream.init(this.outputstream, 'UTF-8', BUFFER_SIZE,
                    Ci.nsIConverterInputStream.DEFAULT_REPLACEMENT_CHARACTER);
      this.stream = transport.openInputStream(0, 0, 0);
      this.instream = Cc['@mozilla.org/intl/converter-input-stream;1']
          .createInstance(Ci.nsIConverterInputStream);
      this.instream.init(this.stream, 'UTF-8', BUFFER_SIZE,
                    Ci.nsIConverterInputStream.DEFAULT_REPLACEMENT_CHARACTER);
  } catch(e) {
      log('jsbridge: Error: ' + e);
  }
  
  this.pump = Cc['@mozilla.org/network/input-stream-pump;1']
      .createInstance(Ci.nsIInputStreamPump);
  this.pump.init(this.stream, -1, -1, 0, 0, false);
  this.pump.asyncRead(new AsyncRead(this), null);
}
Session.prototype.onOutput = function(string) {
  if (typeof(string) != "string") {
    throw "This is not a string"
  } 
  try {
    var stroffset = 0;
    do {
      var parts = '';
      // Handle the case where we are writing something larger than our buffer
      if (string.length > BUFFER_SIZE) {
        parts = string.slice(stroffset, stroffset + BUFFER_SIZE);
      } else {
        parts = string;
      }

      // Update our offset
      stroffset = stroffset += parts.length;

      // write it
      this.outstream.writeString(parts);
    } while (stroffset < string.length);

    // Ensure the entire stream is flushed
    this.outstream.flush();
  } catch (e) {
    throw "JSBridge cannot write: "+string;
  }
};
Session.prototype.onQuit = function() {
  this.instream.close();
  this.outstream.close();
  sessions.remove(session);
};
Session.prototype.encodeOut = function (obj) {
  try {
    this.onOutput(JSON.stringify(obj));
  } catch(e) {
    if (typeof(e) == "string") {
      var exception = e;
    } else {
      var exception = {'name':e.name, 'message':e.message};
    }
    this.onOutput(JSON.stringify({'result':false, 'exception':exception}));
  }
  
}
Session.prototype.receive = function(data) {
  Components.utils.evalInSandbox(data, this.sandbox);
}

var sessions = {
    _list: [],
    add: function(session) {
        this._list.push(session);
    },
    remove: function(session) {
        var index = this._list.indexOf(session);
        if(index != -1)
            this._list.splice(index, 1);
    },
    get: function(index) {
        return this._list[index];
    },
    quit: function() {
        this._list.forEach(function(session) {
          session.onQuit();
        });
        this._list.splice(0, this._list.length);
    }
};

function Server (port) {
  this.port = port;
}
Server.prototype.start = function () {
  try {
    this.serv = Cc['@mozilla.org/network/server-socket;1']
        .createInstance(Ci.nsIServerSocket);
    this.serv.init(this.port, true, -1);
    this.serv.asyncListen(this);
  } catch(e) {
    log('jsbridge: Exception: ' + e);
  }    
}
Server.prototype.stop = function () {
    this.serv.close();
    this.sessions.quit();
    this.serv = undefined;
    
    // If we have file logging turned on, turn it off
    if (gJsbridgeFileLogger) {
      gJsbridgeFileLogger.close();
      gJsbridgeFileLogger = null;
    }
}
Server.prototype.onStopListening = function (serv, status) {
// Stub function
}
Server.prototype.onSocketAccepted = function (serv, transport) {
  session = new Session(transport)
  sessions.add(session);
}

function log(msg) {
  if (DEBUG_ON) {
    dump(msg + '\n');
  }
  
  if (DO_FILE_LOGGING) {
    if (!gJsbridgeFileLogger) {
        // TODO
        return;
    }
    gJsbridgeFileLogger.write(msg);
  } 
}

function startServer(port) {
    var server = new Server(port);
    server.start();
    return server;
}
