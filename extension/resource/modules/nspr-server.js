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
 * Mozilla Foundation
 * Portions created by the Initial Developer are Copyright (C) 2010
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Mikeal Rogers <mikeal.rogers@gmail.com>
 * Heather Arthur <fayearthur@gmail.com>
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

var EXPORTED_SYMBOLS = ["Server", "Session", "sessions", "startServer"];

var socket = {}; Components.utils.import("resource://jsbridge/modules/nspr-socket.js", socket);
var bridge = {}; Components.utils.import("resource://jsbridge/modules/bridge.js", bridge);

var hwindow = Components.classes["@mozilla.org/appshell/appShellService;1"]
    .getService(Components.interfaces.nsIAppShellService)
    .hiddenDOMWindow;

backstage = this;

function Session (client) {
  this.client = client;

  var sandbox = Components.utils.Sandbox(backstage);
  sandbox.bridge = new bridge.Bridge(this);
  sandbox.openPreferences = hwindow.openPreferences;

  client.onMessage(function(data) {
    data = toUnicode(data, "utf-8");
    Components.utils.evalInSandbox(data, sandbox);
  });
}
Session.prototype.send = function(string) {
  if (typeof(string) != "string") {
    throw "jsbridge can only send strings";
  }
  this.client.sendMessage(toUnicode(string, 'utf-8'));
};
Session.prototype.quit = function() {
  this.client.close();
};
Session.prototype.encodeOut = function (obj) {
  try {
    this.send(JSON.stringify(obj));
  } catch(e) {
    if (typeof(e) == "string") {
      var exception = e;
    } else {
      var exception = {'name':e.name, 'message':e.message};
    }
    this.send(JSON.stringify({'result':false, 'exception':exception}));
  }
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
          session.quit();
        });
        this._list.splice(0, this._list.length);
    }
};

var Server = function(port) {
    this.server = new socket.ServerSocket(port);
}

Server.prototype.start = function () {
  this.server.onConnect(function(client) {
    sessions.add(new Session(client));
  });
}

Server.prototype.stop = function () {
    this.server.close();
    sessions.quit();
    this.server = undefined;
}

function startServer(port) {
    var server = new Server(port);
    server.start();
    return server;
}

var toUnicode = function(text, charset) {
  var converter = Components.classes["@mozilla.org/intl/scriptableunicodeconverter"]
                  .createInstance(Components.interfaces.nsIScriptableUnicodeConverter);
  converter.charset = charset;
  text = converter.ConvertToUnicode(text);
  return text;
}