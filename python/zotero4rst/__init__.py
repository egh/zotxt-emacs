"""
  Module
"""
# -*- coding: utf-8 -*-
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

import zotero4rst.json
from zotero4rst.util import html2rst, unquote

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
        onlyZotero = True
        for child in node.children:
            if not isZoteroCite(child):
                onlyZotero = False
        if onlyZotero:
            ## Abuse attributes segment
            node.attributes['onlyZotero'] = True
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

class ZoteroCitationInfo(object):
    """Class to hold information about a citation for passing to Zotero."""
    def __init__(self, **kwargs):
        global zotero_thing
        self.key = kwargs['key']
        newkey = zotero_thing.lookup_key(self.key)
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
Examples: `see @Doe2008` `also c.f. @Doe2010`
Returns an array of hashes with information."""
    global zotero_thing

    KEY_RE = r'-?@([A-Za-z0-9_-]+),?'
    def is_key(s): return re.match(KEY_RE, s)
    def not_is_key(s): return not(is_key(s))
    def not_is_sc(s): return s != ';'

    words = [ n for n in re.split(r"( |;)", cite_string) if n != ' ' and n != '' ]
    raw_keys = [ word for word in words if is_key(word) ]
    if len(raw_keys) == 0:
        raise ExtensionOptionError("No key found in citation: '%s'."%(cite_string))
    elif len(raw_keys) > 1:
        raise ExtensionOptionError("Too many keys in citation: '%s'."%(cite_string))
    else:
        key = re.match(KEY_RE, raw_keys[0]).group(1)
        prefix = " ".join(takewhile(not_is_key, words))
        after_cite = tuple(islice(dropwhile(not_is_key, words), 1, None))
        suffix=None
        locator=None
        # suffix is separate by ;, for now
        locator = " ".join(takewhile(not_is_sc, after_cite))
        suffix = " ".join(islice(dropwhile(not_is_sc, after_cite), 1, None))
        return ZoteroCitationInfo(key=key,
                                  indexNumber=len(zotero_thing.cite_list),
                                  prefix=prefix,
                                  suffix=suffix,
                                  locator=locator)

def zot_cite_role(role, rawtext, text, lineno, inliner,
                  options={}, content=[]):
    global zotero_thing
    check_zotero_thing()

    cite_info = zot_parse_cite_string(text)
    zotero_thing.cite_list.append([cite_info])
    zotero_thing.track_item(cite_info)
    pending = nodes.pending(ZoteroTransform)
    pending.details['zoteroCitation'] = True
    inliner.document.note_pending(pending)
    return [pending], []

# setup zotero directives
directives.register_directive('zotero-setup', ZoteroSetupDirective)
directives.register_directive('zotero', ZoteroDirective)
directives.register_directive('zotero-bibliography', ZoteroBibliographyDirective)
roles.register_canonical_role('zc', zot_cite_role)
