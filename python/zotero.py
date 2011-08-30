"""
  Module
"""
# -*- coding: utf-8 -*-
import BeautifulSoup
import ConfigParser
import json
import os
import re
import sys

import jsbridge

from docutils import nodes
from docutils.parsers.rst import Directive, directives, roles
from docutils.transforms import TransformError, Transform
from docutils.utils import ExtensionOptionError

from itertools import chain, dropwhile, islice, takewhile

import urllib

class smallcaps(nodes.Inline, nodes.TextElement): pass
roles.register_local_role("smallcaps", smallcaps)

DEFAULT_CITATION_FORMAT = "http://www.zotero.org/styles/chicago-author-date"

### How to set up custom text role in zotero.py?

# placeholder for global bridge to Zotero
zotero_thing = None;

# verbose flag
verbose_flag = False

# processing progress flags
started_recording_ids = False
started_transforming_cites = False

# for tracking mobile nodes
footnodes = []
footnode_pos = 0
autonomous_mobile_footnode_indexes = []

def z4r_debug(what):
    global verbose_flag
    if verbose_flag == 1:
        sys.stderr.write("%s\n"%(what))

def check_zotero_thing():
    global zotero_thing
    if not zotero_thing:
        ## A kludge, but makes a big noise about the extension syntax for clarity.
        sys.stderr.write("#####\n")
        sys.stderr.write("##\n")
        sys.stderr.write("##  Must set zotero-setup:: directive before zotero:: directive is used.\n")
        sys.stderr.write("##\n")
        sys.stderr.write("#####\n")
        raise ExtensionOptionError("must set zotero-setup:: directive before zotero:: directive is used.")

def html2rst (html):
    """
    Transform html to reStructuredText internal representation.

    reStructuredText inline markup cannot be nested. The CSL processor
    does produce nested markup, so we ask the processor to deliver HTML,
    and use this function to convert it to the internal representation.
    It depends on Beautiful Soup.

    Note that the function supports small-caps, with the smallcaps
    node name. The Translator instance used by the Writer that consumes
    the output must be extended to support this node type.
    """
    def cleanString(str):
        """
        Replace HTML entities with character equivalents.

        Only these four characters are encoded as entities by the CSL
        processor when running in HTML mode.
        """
        str = str.replace("&#38;", "&")
        str = str.replace("&#60;", "<")
        str = str.replace("&#32;", ">")
        str = str.replace("&#160;", u"\u00A0")
        return str

    def wrap_text(node_list):
        # in rst text must be wrapped in a paragraph, I believe
        # at least rst2pdf disappears the text if it is not - EGH
        retval = []
        last_was_text = False
        # group text nodes in paragraphs
        for node in node_list:
            if isinstance(node, nodes.Inline) or isinstance(node, nodes.Text):
                if last_was_text:
                    retval[-1] += node
                else:
                    retval.append(nodes.paragraph("","", node))
                    last_was_text = True
            else:
                retval.append(node)
                last_was_text = False
        return retval

    def compact(lst):
        return [ x for x in lst if (x is not None) ]

    def walk(html_node):
        """
        Walk the tree, building a reStructuredText object as we go.
        """
        if html_node is None:
            return None
        elif ((type(html_node) == BeautifulSoup.NavigableString) or (type(html_node) == str) or (type(html_node) == unicode)):
            text = cleanString(unicode(html_node))
            # whitespace is significant in reST, so strip out empty text nodes
            if re.match("^\s+$", text):
                return None
            else:
                return nodes.Text(text, rawsource=text)
        else:
            if (html_node.name == 'span'):
                if (html_node.has_key('style') and (html_node['style'] == "font-style:italic;")):
                    return nodes.emphasis(text="".join([ unicode(walk(c)) for c in html_node.contents ]))
                elif (html_node.has_key('style') and (html_node['style'] == "font-variant:small-caps;")):
                    return smallcaps(text="".join([ unicode(walk(c)) for c in html_node.contents ]))
                else:
                    return compact(walk("".join([ str(c) for c in html_node.contents ])))
            if (html_node.name == 'i'):
                return nodes.emphasis(text="".join([ unicode(walk(c)) for c in html_node.contents ]))
            elif (html_node.name == 'p'):
                children = compact([ walk(c) for c in html_node.contents ])
                return nodes.paragraph("", "", *children)
            elif (html_node.name == 'a'):
                children = compact([ walk(c) for c in html_node.contents ])
                return apply(nodes.reference, ["", ""] + children, { 'refuri' : html_node['href'] })
            elif (html_node.name == 'div'):
                children = compact([ walk(c) for c in html_node.contents ])
                classes = re.split(" ", html_node.get('class', ""))
                return nodes.container("", *wrap_text(children), classes=classes)
    
    doc = BeautifulSoup.BeautifulSoup(html)
    ret = compact([ walk(c) for c in doc.contents ])
    return ret

def unquote(source):
    if type(source) == list:
        return [ unquote(x) for x in source ]
    elif type(source) == dict:
        return dict([ (k, unquote(v)) for (k,v) in source.items()])
    elif (type(source) == unicode) or (type(source) == str) or (type(source) == jsbridge.jsobjects.JSString):
        res = urllib.unquote(source)
        if '%u' in res:
            res = res.replace('%u','\\u').decode('unicode_escape')
        return res
    else:
        return source

def isZoteroCite(node):
    ret = False
    isPending = isinstance(node, nodes.pending)
    isZotero = isPending and node.details.has_key('zoteroCitation')
    if isZotero:
        ret = True
    return ret

class ZoteroConnection(object):
    def __init__(self, **kwargs):
        self.bibType = kwargs.get('bibType', DEFAULT_CITATION_FORMAT)
        self.firefox_connect()
        self.zotero_resource()
        self.tracked_items = []
        self.cite_list = []
        self.cite_pos = 0
        self.keys = ConfigParser.SafeConfigParser()
        self.keys.optionxform = str

    def load_keyfile(self, path):
        self.keys.read(os.path.relpath(path))

    def track_item(self, item):
        self.tracked_items.append(item)

    def firefox_connect(self):
        self.back_channel, self.bridge = jsbridge.wait_and_create_network("127.0.0.1", 24242)
        self.back_channel.timeout = self.bridge.timeout = 60

    def zotero_resource(self):
        self.methods = jsbridge.JSObject(self.bridge, "Components.utils.import('resource://csl/export.js')")

    def generate_rest_bibliography(self):
        """Generate a bibliography of reST nodes."""
        self.methods.registerItemIds([ item.id for item in self.tracked_items ])
        bibdata = unquote(json.loads(self.methods.getBibliographyData()))
        # XXX There is some nasty business here.
        #
        # Some nested nodes come out serialized when run through html2rst, so something
        # needs to be fixed there.
        #
        # More important, we need to figure out how to control formatting
        # in the bib -- hanging indents especially. Probably the simplest thing
        # is just to set some off-the-shelf named style blocks in style.odt, and
        # apply them as more or less appropriate.
        #
        return html2rst("%s%s%s"%(bibdata[0]["bibstart"], "".join(bibdata[1]), bibdata[0]["bibend"]))

    def lookup_key(self, key):
        if self.keys.has_option('keys', key):
            return self.keys.get('keys', key)
        else:
            return None

class MultipleCitationVisitor(nodes.SparseNodeVisitor):
    def visit_pending(self, node):
        global zotero_thing
        children = node.parent.children
        # Start at THIS child's offset.
        for start in range(0, len(children), 1):
            if children[start] == node:
                break
            elif isZoteroCite(node):
                zotero_thing.cite_pos += 1
        for pos in range(start, len(children) - 1, 1):
            
            if isZoteroCite(children[pos]) and isZoteroCite(children[pos + 1]):
                nextIsZoteroCite = True;
                offset = 0
                while nextIsZoteroCite:
                    offset += 1
                    if pos + offset > len(children) - 1:
                        break
                    nextIsZoteroCite = isZoteroCite(children[pos + offset])
                    children[pos + offset].details.pop('zoteroCitation')
                    zotero_thing.cite_list[zotero_thing.cite_pos].append(zotero_thing.cite_list[zotero_thing.cite_pos + 1][0])
                    zotero_thing.cite_list.pop(zotero_thing.cite_pos + 1)
        if isZoteroCite(node):
            zotero_thing.cite_pos += 1
    def depart_pending(self, node):
        pass

class NoteIndexVisitor(nodes.SparseNodeVisitor):
    def __init__(self, *args, **kwargs):
        self.in_note = False
        self.note_count = 0
        nodes.SparseNodeVisitor.__init__(self, *args, **kwargs)

    def visit_pending(self, node):
        global zotero_thing
        if node.details.has_key('zoteroCitation'):
            # Do something about the number
            if self.in_note:
                zotero_thing.cite_list[zotero_thing.cite_pos][0].noteIndex = self.note_count
            zotero_thing.cite_pos += 1

    def depart_pending(self, node):
        pass

    def visit_footnote(self, node):
        self.in_note = True
        self.note_count += 1

    def depart_footnote(self, node):
        global footnodes
        onlyZotero = True
        for child in node.children:
            if not isZoteroCite(child):
                onlyZotero = False
        if onlyZotero:
            ## Abuse attributes segment
            node.attributes['onlyZotero'] = True
        footnodes.append(node)
        self.in_note = False

class ZoteroSetupDirective(Directive):
    def __init__(self, *args, **kwargs):
        global zotero_thing, verbose_flag
        Directive.__init__(self, *args)
        # This is necessary: connection hangs if created outside of an instantiated
        # directive class.
        zotero_thing = ZoteroConnection()
        verbose_flag = self.state_machine.reporter.report_level

    required_arguments = 0
    optional_arguments = 0
    has_content = False
    option_spec = {'format' : directives.unchanged,
                   'keyfile': directives.unchanged }
    def run(self):
        global zotero_thing
        if self.options.has_key('keyfile'):
            zotero_thing.load_keyfile(self.options['keyfile'])
        z4r_debug("=== Zotero4reST: Setup run #1 (establish connection, spin up processor) ===")
        zotero_thing.methods.instantiateCiteProc(self.options.get('format', DEFAULT_CITATION_FORMAT))
        pending = nodes.pending(ZoteroSetupTransform)
        pending.details.update(self.options)
        self.state_machine.document.note_pending(pending)
        return [pending]

class ZoteroSetupTransform(Transform):
    default_priority = 500
    def apply(self):
        global zotero_thing
        z4r_debug("\n=== Zotero4reST: Setup run #2 (load IDs to processor) ===")
        self.startnode.parent.remove(self.startnode)
        visitor = NoteIndexVisitor(self.document)
        self.document.walkabout(visitor)
        zotero_thing.cite_pos = 0
        visitor = MultipleCitationVisitor(self.document)
        self.document.walkabout(visitor)
        zotero_thing.cite_pos = 0

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
        global zotero_thing, verbose_flag, started_recording_ids
        check_zotero_thing()

        if verbose_flag == 1 and not started_recording_ids:
            sys.stderr.write("--- Zotero4reST: Citation run #1 (record ID) ---\n")
            started_recording_ids = True
        for key in ['locator', 'label', 'prefix', 'suffix']:
            if not self.options.has_key(key):
                self.options[key] = ''
        # The noteIndex and indexNumber belong in properties,
        # but we fudge that in this phase, before citations are
        # composed -- we'll pull the values out of the first cite in
        # the cluster in the composition pass.

        details = ZoteroCitationInfo(key=self.arguments[0],
                                     indexNumber=len(zotero_thing.cite_list),
                                     locator=self.options['locator'],
                                     label=self.options['label'],
                                     prefix=self.options['prefix'],
                                     suffix=self.options['suffix'])
        zotero_thing.track_item(details)
        zotero_thing.cite_list.append([details])
        pending = nodes.pending(ZoteroTransform)
        pending.details.update(self.options)
        pending.details['zoteroCitation'] = True
        self.state_machine.document.note_pending(pending)
        if verbose_flag == 1:
            sys.stderr.write(".")
            sys.stderr.flush()
        return [pending]

class ZoteroTransform(Transform):
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

    def apply(self):
        global zotero_thing, note_number, verbose_flag, started_transforming_cites
        if not self.startnode.details.has_key('zoteroCitation'):
            self.startnode.parent.remove(self.startnode)
            if verbose_flag == 1:
                sys.stderr.write("<")
                sys.stderr.flush()
        else:
            if verbose_flag == 1 and not started_transforming_cites:
                sys.stderr.write("--- Zotero4reST: Citation run #2 (render cite and insert) ---\n")
                started_transforming_cites = True
            citation = {
                'citationItems':zotero_thing.cite_list[zotero_thing.cite_pos],
                'properties': {
                    'index': zotero_thing.cite_pos,
                    'noteIndex': zotero_thing.cite_list[zotero_thing.cite_pos][0].noteIndex
                }
            }
            res = zotero_thing.methods.getCitationBlock(citation)
            zotero_thing.cite_pos += 1
            newnode = html2rst(unquote(res))
            # wrap in a paragraph if it is a note
            if not(zotero_thing.methods.isInTextStyle()):
                newnode = nodes.paragraph('', '', *newnode)
            if verbose_flag == 1:
                sys.stderr.write(".")
                sys.stderr.flush()

            moved = False
            parent = self.startnode.parent
            for pos in range(len(parent.children)-1, -1, -1):
                if parent.children[pos] == self.startnode:
                    if pos < len(parent.children) - 1 and isinstance(parent.children[pos + 1], nodes.paragraph):
                        quashed_period = False
                        following_para = parent.children[pos + 1]
                        if len(following_para.children) and isinstance(following_para.children[0], nodes.Text):
                            if following_para.children[0].rawsource[0] in [",", ";"]:
                                if isinstance(newnode.children[-1], nodes.Text):
                                    if newnode.children[-1].rawsource[-1] == ".":
                                        raw = newnode.children[-1].rawsource
                                        newchild = nodes.Text(raw[:-1], rawsource=raw[:-1])
                                        newnode.remove(newnode.children[-1])
                                        newnode += newchild
                                        quashed_period = True
                        if not quashed_period:
                            newnode += nodes.Text(" ")
                        for child in following_para.children:
                            newnode += child
                        parent.remove(following_para)
                    if pos > 0 and isinstance(parent.children[pos - 1], nodes.paragraph):
                        parent.children[pos - 1] += nodes.generated("", " ")
                        for mychild in newnode:
                            parent.children[pos - 1] += mychild
                        moved = True
            if moved:
                self.startnode.parent.remove(self.startnode)
            else:
                self.startnode.replace_self(newnode)

class ZoteroBibliographyDirective(Directive):

    ## This could be extended to support selection of
    ## included bibliography entries. The processor has
    ## an API to support this, although it hasn't yet been
    ## implemented in any products that I know of.
    required_arguments = 0
    optional_arguments = 1
    has_content = False
    def run(self):
        z4r_debug("\n--- Zotero4reST: Bibliography #1 (placeholder set) ---")
        pending = nodes.pending(ZoteroBibliographyTransform)
        pending.details.update(self.options)
        self.state_machine.document.note_pending(pending)
        return [pending]

class ZoteroBibliographyTransform(Transform):

    default_priority = 530

    def apply(self):
        z4r_debug("\n--- Zotero4reST: Bibliography #2 (inserting content) ---")
        newnode = zotero_thing.generate_rest_bibliography()
        self.startnode.replace_self(newnode)

class ZoteroJSONEncoder(jsbridge.network.JSObjectEncoder):
    """An encoder for our JSON objects."""
    def default(self, obj):
        if isinstance(obj, ZoteroCitationInfo):
            return { 'id'          : obj.id,
                     'indexNumber' : obj.indexNumber,
                     'label'       : obj.label,
                     'locator'     : obj.locator,
                     'noteIndex'   : obj.noteIndex,
                     'prefix'      : obj.prefix,
                     'suffix'      : obj.suffix }
        else: return json.JSONEncoder.default(self, obj)

jsbridge.network.encoder = ZoteroJSONEncoder()

class ZoteroCitationInfo(object):
    """Class to hold information about a citation for passing to Zotero."""
    def __init__(self, **kwargs):
        global zotero_thing
        self.key = kwargs['key']
o        newkey = zotero_thing.lookup_key(self.key)
        if newkey is not None:
            self.key = newkey
        self.id = int(zotero_thing.methods.getItemId(self.key))
        self.indexNumber = kwargs.get('indexNumber', None)
        self.label = kwargs.get('label', None)
        self.locator = kwargs.get('locator', None)
        self.noteIndex = kwargs.get('noteIndex', 0)
        self.prefix = kwargs.get('prefix', None)
        self.suffix = kwargs.get('suffix', None)

def zot_parse_cite_string(cite_string):
    """Parse a citation string. This is inteded to be "pandoc-like".
Examples: `see @Doe2008; also c.f. @Doe2010`
Returns an array of hashes with information."""
    global zotero_thing

    KEY_RE = r'-?@([A-Z0-9]+)'
    def is_key(s): return re.match(KEY_RE, s)
    def not_is_key(s): return not(is_key(s))

    cite_string_list = re.split(r'; *', cite_string)
    retval = []
    for cite in cite_string_list:
        words = re.split(r' ', cite)
        raw_keys = [ word for word in words if is_key(word) ]
        if len(raw_keys) == 0:
            raise ExtensionOptionError("No key found in citation: '%s'."%(cite_string))
        elif len(raw_keys) > 1:
            raise ExtensionOptionError("Too many keys in citation: '%s'."%(cite_string))
        else:
            retval.append(ZoteroCitationInfo(key=re.match(KEY_RE, raw_keys[0]).group(1),
                                             indexNumber=len(zotero_thing.cite_list), 
                                             prefix=" ".join(takewhile(not_is_key, words)),
                                             suffix=" ".join(islice(dropwhile(not_is_key, words), 1, None))))
    return retval

def zot_cite_role(role, rawtext, text, lineno, inliner,
                  options={}, content=[]):
    global zotero_thing
    check_zotero_thing()

    pending_list = []
    cites = zot_parse_cite_string(text)
    for cite_info in cites:
        zotero_thing.cite_list.append([cite_info])
        zotero_thing.track_item(cite_info)
        pending = nodes.pending(ZoteroTransform)
        pending.details['zoteroCitation'] = True
        inliner.document.note_pending(pending)
        pending_list.append(pending)
    return pending_list, []

# setup zotero directives
directives.register_directive('zotero-setup', ZoteroSetupDirective)
directives.register_directive('zotero', ZoteroDirective)
directives.register_directive('zotero-bibliography', ZoteroBibliographyDirective)
roles.register_canonical_role('zc', zot_cite_role)
