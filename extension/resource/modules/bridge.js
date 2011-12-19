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


var EXPORTED_SYMBOLS = ["Bridge"];

var events = {}; Components.utils.import("resource://jsbridge/modules/events.js", events);

var uuidgen = Components.classes["@mozilla.org/uuid-generator;1"]
    .getService(Components.interfaces.nsIUUIDGenerator);


globalRegistry = {};

function Bridge (session) {
  this.session = session;
  this.registry = globalRegistry;
}
Bridge.prototype._register = function (_type) {
  this.bridgeType = _type;
  if (_type == "backchannel") {
    events.addBackChannel(this);
  }
}
Bridge.prototype.register = function (uuid, _type) {
  try {
    this._register(_type);
    var passed = true;
  } catch(e) {
    if (typeof(e) == "string") {
      var exception = e;
    } else {
      var exception = {'name':e.name, 'message':e.message};
    }
    this.session.encodeOut({'result':false, 'exception':exception, 'uuid':uuid});
  }
  if (passed != undefined) {
    this.session.encodeOut({"result":true, 'eventType':'register', 'uuid':uuid});
  }
  
}
Bridge.prototype._describe = function (obj) {
  var response = {};
  if (obj == null) {
    var type = "null";
  } else {
    var type = typeof(obj);
  }
  if (type == "object") {
    if (obj.length != undefined) {
      var type = "array";
    }
    response.attributes = [];
    for (i in obj) {
      response.attributes = response.attributes.concat(i);
    }
  }
  else if (type != "function"){
    response.data = obj;
  }
  response.type = type;
  return response;
}
Bridge.prototype.describe = function (uuid, obj) {
  var response = this._describe(obj);
  response.uuid = uuid;
  response.result = true;
  this.session.encodeOut(response);
}
Bridge.prototype._set = function (obj) {
  var uuid = uuidgen.generateUUID().toString();
  this.registry[uuid] = obj;
  return uuid;
}
Bridge.prototype.set = function (uuid, obj) {
  var ruuid = this._set(obj);
  this.session.encodeOut({'result':true, 'data':'bridge.registry["'+ruuid+'"]', 'uuid':uuid});
}
Bridge.prototype._setAttribute = function (obj, name, value) {
  obj[name] = value;
  return value;
}
Bridge.prototype.setAttribute = function (uuid, obj, name, value) {
  // log(uuid, String(obj), name, String(value))
  try {
    var result = this._setAttribute(obj, name, value);
  } catch(e) {
    if (typeof(e) == "string") {
      var exception = e;
    } else {
      var exception = {'name':e.name, 'message':e.message};
    }
    this.session.encodeOut({'result':false, 'exception':exception, 'uuid':uuid});
  }
  if (result != undefined) {
    this.set(uuid, obj[name]);
  }
}
Bridge.prototype._execFunction = function (func, args) {
  return func.apply(this.session.sandbox, args);
}
Bridge.prototype.execFunction = function (uuid, func, args) {
  try {
    var data = this._execFunction(func, args);
    var result = true;
  } catch(e) {
    if (typeof(e) == "string") {
      var exception = e;
    } else {
      var exception = {'name':e.name, 'message':e.message};
    }
    this.session.encodeOut({'result':false, 'exception':exception, 'uuid':uuid});
    var result = true;
  }  
  if (data != undefined) {
    this.set(uuid, data);
  } else if ( result == true) {
    this.session.encodeOut({'result':true, 'data':null, 'uuid':uuid});
  } else {
    throw 'jsbridge could not execute function ' + func;
  }
}