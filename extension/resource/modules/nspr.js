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
 
Components.utils.import("resource://gre/modules/Services.jsm");
Components.utils.import("resource://gre/modules/ctypes.jsm");

var EXPORTED_SYMBOLS = ["nsprSockets", "nsprTypes"];
 
// Open the NSSPR library.
var nsprfile = Services.dirsvc.get("GreD", Components.interfaces.nsILocalFile);
nsprfile.append(ctypes.libraryName("nspr4"));
var lib = ctypes.open(nsprfile.path);

var nsprTypes = {
  PRFileDesc : ctypes.StructType("PRFileDesc"),

  PRNetAddr : ctypes.StructType("PRNetAddr", 
                                  [{'family': ctypes.uint16_t},
                                   {'port': ctypes.uint16_t},
                                   {'ip': ctypes.uint32_t},
                                   {'pad' : ctypes.char.array(8)}]),

  PRSocketOptionData : ctypes.StructType("PRSocketOptionData",
                                          [{'option' : ctypes.int32_t},
                                           {'non_blocking': ctypes.int32_t}])
};

var nsprSockets = {
  PR_TRUE : 1,
  PR_AF_INET : 2,
  PR_IpAddrAny : 1,
  PR_IpAddrLoopback : 2,
  PR_SockOpt_Nonblocking : 0,
  PR_SockOpt_Reuseaddr: 2,
  PR_SockOpt_NoDelay: 13,
  PR_INTERVAL_NO_WAIT : 0,
  PR_INTERVAL_MAX : 100000,

  buffer : ctypes.ArrayType(ctypes.char),

  PR_SetNetAddr : lib.declare("PR_SetNetAddr",
                         ctypes.default_abi,
                         ctypes.int32_t, // really doesn't return anything
                         ctypes.int32_t, // val
                         ctypes.uint16_t, // af
                         ctypes.uint16_t, // port
                         nsprTypes.PRNetAddr.ptr),

  PR_OpenTCPSocket : lib.declare("PR_OpenTCPSocket",     // symbol name
                            ctypes.default_abi,             // cdecl calling convention
                            nsprTypes.PRFileDesc.ptr,  // return (PRFileDesc*)
                            ctypes.int32_t),                 // first arg
                        
  PR_SetSocketOption : lib.declare("PR_SetSocketOption",
                              ctypes.default_abi,
                              ctypes.int32_t,
                              nsprTypes.PRFileDesc.ptr,
                              nsprTypes.PRSocketOptionData.ptr),

  PR_Bind : lib.declare("PR_Bind",
                            ctypes.default_abi,
                            ctypes.int32_t,
                            nsprTypes.PRFileDesc.ptr,
                            nsprTypes.PRNetAddr.ptr),
                        
  PR_Listen : lib.declare("PR_Listen",
                              ctypes.default_abi,
                              ctypes.int32_t,
                              nsprTypes.PRFileDesc.ptr, // fd
                              ctypes.int32_t),  // backlog
                          
  PR_Accept : lib.declare("PR_Accept",
                              ctypes.default_abi,
                              nsprTypes.PRFileDesc.ptr, // new socket fd
                              nsprTypes.PRFileDesc.ptr, // rendezvous socket fd
                              nsprTypes.PRNetAddr.ptr, //addr
                              ctypes.uint32_t), // timeout interval
                         
  PR_Close : lib.declare("PR_Close",
                          ctypes.default_abi,
                          ctypes.int32_t,
                          nsprTypes.PRFileDesc.ptr),
                         
  PR_Recv : lib.declare("PR_Recv",
                          ctypes.default_abi,
                          ctypes.int32_t, // return
                          nsprTypes.PRFileDesc.ptr, // socket
                          ctypes.voidptr_t, // buffer
                          ctypes.int32_t, // buffer length
                          ctypes.int32_t, // must be 0, deprecated
                          ctypes.uint32_t), // timeout interval
                        
  PR_Send : lib.declare("PR_Send",
                          ctypes.default_abi,
                          ctypes.int32_t, // return
                          nsprTypes.PRFileDesc.ptr, // socket
                          ctypes.voidptr_t, // buffer
                          ctypes.int32_t, // buffer length
                          ctypes.int32_t, // must be 0, deprecated
                          ctypes.uint32_t), // timeout interval
}