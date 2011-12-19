var EXPORTED_SYMBOLS = ["JsbridgeFileLogger"];

var Cc = Components.classes;
var Ci = Components.interfaces;

var gJsbridgeFileLogger = null;

function JsbridgeFileLogger(filename) {
  this.filename = filename;
  this.init();
}
JsbridgeFileLogger.prototype = {
  _file: null,
  _fostream: null,
  init: function jsbfli() {
    this._file = Cc["@mozilla.org/file/local;1"].createInstance(Ci.nsILocalFile);
    this._file.initWithPath(this.filename);
    this._fostream = Cc["@mozilla.org/network/file-output-stream;1"]
                    .createInstance(Ci.nsIFileOutputStream);
    // Create with PR_WRITE_ONLY(0x02), PR_CREATE_FILE(0x08), PR_APPEND(0x10)
    this._fostream.init(this._file, 0x02 | 0x08 | 0x10, 0664, 0);
  },
  write: function jsbflw(msg) {
    if (this._fostream) {
      var msgn = msg + "\n";
      this._fostream.write(msgn, msgn.length);
    }
  },
  close: function jsbflc() {
    if (this._fostream)
      this._fostream.close();
    this._fostream = null;
    this._file = null;
  }
}; 

