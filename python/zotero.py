"""
  Module
"""
# -*- coding: utf-8 -*-

from jsbridge import wait_and_create_network, JSObject

from docutils import nodes
from docutils.parsers.rst import Directive
from docutils.parsers.rst import directives
from itertools import chain
from docutils.transforms import TransformError, Transform
from urllib import unquote
import BeautifulSoup

citation_format = "http://www.zotero.org/styles/chicago-author-date"

# unique items
item_list = []
item_array = {}

# everything, in sequence
cite_list = []
cite_pos = 0

# variables for use in final assignment of note numbers
# (see CitationVisitor)
in_note = False
note_count = 0

# placeholder for global bridge to Zotero
zotero_thing = None;

# verbose flag
verbose_flag = False

def html2rst (html):
    def walk(node, atTop=False):
        if node == None:
            return nodes.Text("")
        elif ((type(node) == BeautifulSoup.NavigableString) or (type(node) == str) or (type(node) == unicode)):
            if atTop:
                return nodes.generated('', unicode(node))
            else:
                return nodes.Text(unicode(node))
        else:
            if (node.name == 'span'):
                if (node.has_key('style') and (node['style'] == "font-style:italic;")):
                    return nodes.emphasis(text="".join([ unicode(walk(c)) for c in node.contents ]))
                else:
                    return walk("".join([ str(c) for c in node.contents ]))
            if (node.name == 'i'):
                #print node
                return nodes.emphasis(text="".join([ unicode(walk(c)) for c in node.contents ]))
            elif (node.name == 'p'):
                children = [ walk(c) for c in node.contents ]
                return nodes.paragraph("", "", *children)
            elif (node.name == 'a'):
                children = [ walk(c) for c in node.contents ]
                return apply(nodes.reference, ["", ""] + children, { 'refuri' : node['href'] })
            elif (node.name == 'div'):
                children = [ walk(c) for c in node.contents ]
                return nodes.paragraph("", "", *children)
    doc = BeautifulSoup.BeautifulSoup(html)
    return [ walk(c, True) for c in doc.contents ]

class MultipleCitationVisitor(nodes.SparseNodeVisitor):
    def visit_pending(self, node):
        global cite_pos
        children = node.parent.children
        for pos in range(0, len(children) - 1, 1):
            thisIsPending = isinstance(children[pos], nodes.pending)
            thisIsZotero = children[pos].details.has_key('zoteroCitation')
            nextIsPendingToo = isinstance(children[pos + 1], nodes.pending)
            nextIsZotero = children[pos + 1].details.has_key('zoteroCitation')
            
            if thisIsPending and thisIsZotero:
                offset = 0
                while nextIsPendingToo and nextIsZotero:
                    offset += 1
                    if pos + offset > len(children) - 1:
                        break

                    nextIsPendingToo = isinstance(children[pos + offset], nodes.pending)
                    nextIsZotero = children[pos + offset].details.has_key('zoteroCitation')

                    children[pos + offset].details.pop('zoteroCitation')
                    cite_list[cite_pos].append(cite_list[cite_pos + 1][0])
                    cite_list.pop(cite_pos + 1)
        cite_pos += 1
    def depart_pending(self, node):
        pass

class NoteIndexVisitor(nodes.SparseNodeVisitor):
    def visit_pending(self, node):
        global cite_list, cite_pos, in_note, note_count
        if node.details.has_key('zoteroCitation'):
            # Do something about the number
            if in_note:
                cite_list[cite_pos][0]['noteIndex'] = note_count
            cite_pos += 1
    def depart_pending(self, node):
        pass
    def visit_footnote(self, node):
        global in_note, note_count
        in_note = True
        note_count += 1
    def depart_footnote(self, node):
        global in_note
        in_note = False

class ZoteroConnection(object):
    def __init__(self, **kwargs):
        self.bibType = kwargs.get('bibType', citation_format)
        self.firefox_connect()
        self.zotero_resource()

    def firefox_connect(self):
        self.back_channel, self.bridge = wait_and_create_network("127.0.0.1", 24247)
        self.back_channel.timeout = self.bridge.timeout = 60

    def zotero_resource(self):
        self.methods = JSObject(self.bridge, "Components.utils.import('resource://zotero-for-restructured-text/modules/export.js')")


class ZoteroSetupDirective(Directive, ZoteroConnection):
    from docutils.parsers.rst.directives import unchanged
    def __init__(self, *args):
        global zotero_thing, verbose_flag
        Directive.__init__(self, *args)
        # This is necessary: connection hangs if created outside of an instantiated
        # directive class.
        ZoteroConnection.__init__(self)
        zotero_thing = self.methods
        verbose_flag = self.state_machine.reporter.report_level

    required_arguments = 0
    optional_arguments = 0
    has_content = False
    option_spec = {'format': unchanged}
    def run(self):
        global citation_format, zotero_thing, verbose_flag
        if verbose_flag:
            print "=== Zotero4reST: Setup run #1 (establish connection, spin up processor) ==="
        if self.options.has_key('format'):
            citation_format = self.options['format']
        zotero_thing.instantiateCiteProc(citation_format);
        pending = nodes.pending(ZoteroSetupTransformDirective)
        pending.details.update(self.options)
        self.state_machine.document.note_pending(pending)
        return [pending]

class ZoteroSetupTransformDirective(Transform):
    default_priority = 500
    def apply(self):
        global zotero_thing, cite_pos, verbose_flag
        if verbose_flag:
            print "=== Zotero4reST: Setup run #2 (load IDs to processor) ==="
        zotero_thing.registerItemIds(item_list)
        self.startnode.parent.remove(self.startnode)
        ## Here we walk the document, checking note state and
        ## setting noteIndex value as we go along.
        visitor = NoteIndexVisitor(self.document)
        self.document.walkabout(visitor)
        cite_pos = 0
        visitor = MultipleCitationVisitor(self.document)
        self.document.walkabout(visitor)
        cite_pos = 0

class ZoteroDirective(Directive):
    """
    Zotero cite.

    Zotero cites are generated in two passes: initial parse and
    transform. During the initial parse, a 'pending' element is
    generated which acts as a placeholder, storing the cite ID and
    any options internally.  At a later stage in the processing, the
    'pending' element is replaced by something else.
    """

    required_arguments = 1
    optional_arguments = 1
    final_argument_whitespace = True
    has_content = False
    option_spec = {'locator': directives.unchanged,
                   'label': directives.unchanged,
                   'prefix': directives.unchanged,
                   'suffix': directives.unchanged,
                   'suppress-author': directives.flag}

    def run(self):
        global citation_format, item_list, item_array, zotero_thing, cite_list, verbose_flag
        # XXX Should complain if zotero_setup:: declaration was missing.
        if verbose_flag:
            print "--- Zotero4reST: Citation run #1 (record ID) ---"
        itemID = int(zotero_thing.getItemId(self.arguments[0]))
        for key in ['locator', 'label', 'prefix', 'suffix']:
            if not self.options.has_key(key):
                self.options[key] = ''
        # The noteIndex and indexNumber belong in properties,
        # but we fudge that in this phase, before citations are
        # composed -- we'll pull the values out of the first cite in
        # the cluster in the composition pass.

        details = {
            'id':itemID,
            'noteIndex':0,
            'indexNumber': len(cite_list),
            'locator': self.options['locator'],
            'label': self.options['label'],
            'prefix': self.options['prefix'],
            'suffix': self.options['suffix']
        }
        if not item_array.has_key(itemID):
            item_array[itemID] = True
            item_list.append(itemID)
        cite_list.append([details])
        pending = nodes.pending(ZoteroTransformDirective)
        pending.details.update(self.options)
        pending.details['zoteroCitation'] = True
        self.state_machine.document.note_pending(pending)
        return [pending]

class ZoteroTransformDirective(Transform):
    default_priority = 510
    # Bridge hangs if output contains above-ASCII chars (I guess Telnet kicks into
    # binary mode in that case, leaving us to wait for a null string terminator)
    # JS strings are in Unicode, and the JS escaping mechanism for Unicode with
    # escape() is, apparently, non-standard. I played around with various
    # combinations of decodeURLComponent() / encodeURIComponent() and escape() /
    # unescape() ... applying escape() on the JS side of the bridge, and
    # using the following suggestion for a Python unquote function worked,
    # so I stuck with it:
    #   http://stackoverflow.com/questions/300445/how-to-unquote-a-urlencoded-unicode-string-in-python
    def unquote_u(self,source):
        res = unquote(source)
        if '%u' in res:
            res = res.replace('%u','\\u').decode('unicode_escape')
        return res

    def apply(self):
        global note_number, cite_pos, cite_list, verbose_flag
        if not self.startnode.details.has_key('zoteroCitation'):
            self.startnode.parent.remove(self.startnode)
        else:
            if verbose_flag:
                print "--- Zotero4reST: Citation run #2 (render cite and insert) ---"
            citation = {
                'citationItems':cite_list[cite_pos],
                'properties': {
                    'index': cite_pos,
                    'noteIndex': cite_list[cite_pos][0]['noteIndex']
                }
            }
            res = zotero_thing.getCitationBlock(citation)
            cite_pos += 1
            mystr = self.unquote_u(res)
            newnode = html2rst(mystr)
            self.startnode.replace_self(newnode)
