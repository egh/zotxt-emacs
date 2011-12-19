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
 
var nspr = {}; Components.utils.import("resource://jsbridge/modules/nspr.js", nspr);
var nsprTypes = nspr.nsprTypes;
nspr = nspr.nsprSockets;

var hwindow = Components.classes["@mozilla.org/appshell/appShellService;1"]
                .getService(Components.interfaces.nsIAppShellService)
                .hiddenDOMWindow;

var EXPORTED_SYMBOLS = ["ServerSocket"];
 
var ServerSocket = function(port) {
  var addr = nsprTypes.PRNetAddr();
  nspr.PR_SetNetAddr(nspr.PR_IpAddrLoopback, nspr.PR_AF_INET,
                     port, addr.address());

  var fd = nspr.PR_OpenTCPSocket(nspr.PR_AF_INET);

  // don't block for accept/send/recv
  var opt = nsprTypes.PRSocketOptionData();
  opt.non_blocking = nspr.PR_TRUE;
  opt.option = nspr.PR_SockOpt_Nonblocking;
  nspr.PR_SetSocketOption(fd, opt.address());
  
  // don't buffer when sending
  var opt = nsprTypes.PRSocketOptionData();
  opt.non_blocking = nspr.PR_TRUE; // same space
  opt.option = nspr.PR_SockOpt_NoDelay;
  nspr.PR_SetSocketOption(fd, opt.address());

  // allow local address re-use
  var opt = nsprTypes.PRSocketOptionData();
  opt.non_blocking = nspr.PR_TRUE; // same space
  opt.option = nspr.PR_SockOpt_Reuseaddr;
  nspr.PR_SetSocketOption(fd, opt.address());

  var status = nspr.PR_Bind(fd, addr.address());
  if(status != 0)
    throw "socket failed to bind, kill all firefox processes";

  var status = nspr.PR_Listen(fd, -1);
  if(status != 0)
    throw "socket failed to listen";

  this.addr = addr;
  this.fd = fd;
}

ServerSocket.prototype = {
  onConnect : function(callback, interval) {
    interval = interval || 300;
    var that = this;
    (function accept() {
      var newfd = nspr.PR_Accept(that.fd, that.addr.address(), nspr.PR_INTERVAL_NO_WAIT);
      if(!newfd.isNull())
        callback(new Client(newfd));
      hwindow.setTimeout(accept, interval);
    })();
  },

  close : function() {
    return nspr.PR_Close(this.fd); 
  }
}


var Client = function(fd) {
  this.fd = fd;
}

Client.prototype = {
  onMessage : function(callback, interval, bufsize) {
    bufsize = bufsize || 4096;
    interval = interval || 100; // polling interval
    var that = this;

    (function getMessage() {
      var buffer = new nspr.buffer(bufsize);
      var bytes = nspr.PR_Recv(that.fd, buffer, bufsize, 0, nspr.PR_INTERVAL_NO_WAIT);
      if(bytes > 0) {
        var message = buffer.readString();
        callback(message);
      }
      else if(bytes == 0) {
        if(that.handleDisconnect)
          that.handleDisconnect();
        return;
      }
      hwindow.setTimeout(getMessage, interval);
    })();
  },

  onDisconnect : function(callback) {
    this.handleDisconnect = callback;
  },

  sendMessage : function(message) {
    var buffer = new nspr.buffer(message);
    nspr.PR_Send(this.fd, buffer, message.length, 0, nspr.PR_INTERVAL_MAX);
  },

  close : function() {
    return nspr.PR_Close(this.fd);
  }
}
