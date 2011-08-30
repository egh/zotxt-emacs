import jsbridge
import zotero4rst

class ZoteroJSONEncoder(jsbridge.network.JSObjectEncoder):
    """An encoder for our JSON objects."""
    def default(self, obj):
        if isinstance(obj, zotero4rst.ZoteroCitationInfo):
            return { 'id'          : obj.id,
                     'indexNumber' : obj.indexNumber,
                     'label'       : obj.label,
                     'locator'     : obj.locator,
                     'noteIndex'   : obj.noteIndex,
                     'prefix'      : obj.prefix,
                     'suffix'      : obj.suffix }
        else: return json.JSONEncoder.default(self, obj)

jsbridge.network.encoder = ZoteroJSONEncoder()
