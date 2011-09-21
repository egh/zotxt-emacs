import jsbridge
import zotero4rst

class ZoteroJSONEncoder(jsbridge.network.JSObjectEncoder):
    """An encoder for our JSON objects."""
    def default(self, obj):
        if isinstance(obj, zotero4rst.ZoteroCitationInfo):
            retval = { 'id': obj.id}
            if obj.prefix: retval['prefix'] = obj.prefix
            if obj.suffix: retval['suffix'] = obj.suffix
            if obj.label: retval['label'] = obj.label
            if obj.locator: retval['locator'] = obj.locator
            if obj.suppress_author: retval['suppress-author'] = obj.suppress_author
            if obj.author_only: retval['author-only'] = obj.author_only
            return retval
        else: return json.JSONEncoder.default(self, obj)

jsbridge.network.encoder = ZoteroJSONEncoder()
