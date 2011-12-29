import jsbridge
from xciterst import CitationInfo, CitationCluster

class ZoteroJSONEncoder(jsbridge.network.JSObjectEncoder):
    """An encoder for our JSON objects."""
    def default(self, obj):
        if isinstance(obj, CitationInfo):
            retval = { 'id': obj.id}
            if obj.prefix: retval['prefix'] = "%s "%(obj.prefix) # ensure spaces in prefix, suffix
            if obj.suffix: retval['suffix'] = " %s"%(obj.suffix)
            if obj.label: retval['label'] = obj.label
            if obj.locator: retval['locator'] = obj.locator
            if obj.suppress_author: retval['suppress-author'] = obj.suppress_author
            if obj.author_only: retval['author-only'] = obj.author_only
            return retval
        elif isinstance(obj, CitationCluster):
            return { 'citationItems': obj.citations,
                     'properties'   : { 'index'     : obj.index,
                                        'noteIndex' : obj.note_index } }
        else: return json.JSONEncoder.default(self, obj)

jsbridge.network.encoder = ZoteroJSONEncoder()
