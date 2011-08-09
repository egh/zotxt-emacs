import jsbridge

JSBRIDGE_PORT = 24242
JSBRIDGE_TIMEOUT = 60. # timeout for jsbridge

class ZoteroConnection(object):
    
    def __init__(self, jsbridge_port=JSBRIDGE_PORT, jsbridge_timeout=JSBRIDGE_TIMEOUT):
        self.jsbridge_port = jsbridge_port
        self.jsbridge_timeout = jsbridge_timeout
        self.persisted = {};
        
    def firefox_connect(self):
        
        # get the bridge and the back-channel
        self.back_channel, self.bridge = jsbridge.wait_and_create_network("127.0.0.1",
                                                                          self.jsbridge_port)

        # set a timeout on jsbridge actions in order to ensure termination
        # (don't think we'll be needing back_channel)
        self.back_channel.timeout = self.bridge.timeout = self.jsbridge_timeout

    def zotero_resource(self):
        zotero = jsbridge.JSObject(self.bridge, "Components.utils.import('resource://zotero-for-restructured-text/modules/export.js')")
        return zotero
        
if __name__ == '__main__':

    zr = ZoteroConnection()
    zr.firefox_connect()
    z = zr.zotero_resource()

    print dir(z)
    print z
    print z.instantiateCiteproc()
