"""
  Module
"""
# -*- coding: utf-8 -*-
import ConfigParser
import json
import os
import random
import re
import string
import sys

import jsbridge

from docutils import nodes
from docutils.parsers.rst import Directive, directives, roles
from docutils.transforms import TransformError, Transform
from docutils.utils import ExtensionOptionError

from itertools import chain, dropwhile, islice, takewhile

import zot4rst.jsonencoder
from zot4rst.util import html2rst, unquote
from xciterst.parser import CiteParser

class smallcaps(nodes.Inline, nodes.TextElement): pass
roles.register_local_role("smallcaps", smallcaps)

DEFAULT_CITATION_FORMAT = "http://www.zotero.org/styles/chicago-author-date"

# placeholder for global bridge to Zotero
zotero_conn = None;

# verbose flag
verbose_flag = False

def check_zotero_conn():
    if not zot4rst.zotero_conn:
        ## A kludge, but makes a big noise about the extension syntax for clarity.
        sys.stderr.write("#####\n")
        sys.stderr.write("##\n")
        sys.stderr.write("##  Must set zotero-setup:: directive before zotero:: directive is used.\n")
        sys.stderr.write("##\n")
        sys.stderr.write("#####\n")
        raise ExtensionOptionError("must set zotero-setup:: directive before zotero:: directive is used.")

class ZoteroConnection(object):
    def __init__(self, format, **kwargs):
        # connect & setup
        self.back_channel, self.bridge = jsbridge.wait_and_create_network("127.0.0.1", 24242)
        self.back_channel.timeout = self.bridge.timeout = 60
        self.methods = jsbridge.JSObject(self.bridge, "Components.utils.import('resource://citeproc/citeproc.js')")
        self.methods.instantiateCiteProc(format)
        self.in_text_style = self.methods.isInTextStyle()

        # setup key mapping
        self.keymap = ConfigParser.SafeConfigParser()
        self.keymap.optionxform = str

        self.tracked_clusters = []
        self.registered_items = []
        self.key2id = {}
        self.local_items = {}

    def set_format(self, format):
        self.methods.instantiateCiteProc(format)

    def get_item_id(self, key):
        """Returns the id of an item with a given key. Key will be
        looked up in the local keymap before the id is looked up."""

        if self.local_items.has_key(key):
            return "MY-%s"%(key)
        else:            
            if not(self.key2id.has_key(key)):
                self.key2id[key] = int(zot4rst.zotero_conn.methods.getItemId(key))
            return self.key2id[key]

    def load_keymap(self, path):
        self.keymap.read(os.path.relpath(path))

    def track_cluster(self, cluster):
        self.tracked_clusters.append(cluster)

    def register_items(self):
        def flatten(listoflists):
            return chain.from_iterable(listoflists)

        uniq_ids = set([ self.get_item_id(item.key)
                         for item in flatten(self.tracked_clusters) ])
        if (uniq_ids != self.registered_items):
            self.methods.registerItemIds(list(uniq_ids))
            self.registered_items = uniq_ids

    def get_index(self, cluster):
        return self.tracked_clusters.index(cluster)

    def generate_rest_bibliography(self):
        """Generate a bibliography of reST nodes."""
        self.register_items()
        data = self.methods.getBibliographyData()
        if not(data):
            return html2rst("")
        else:
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
            bibdata = unquote(json.loads(data))
            return html2rst("%s%s%s"%(bibdata[0]["bibstart"], "".join(bibdata[1]), bibdata[0]["bibend"]))

    def lookup_key(self, key):
        if self.keymap.has_option('keymap', key):
            # return only the first part, the real key - rest is comment
            return re.match("^([0-9A-Z_]+)", self.keymap.get('keymap', key)).group(1)
        else:
            return key

    def get_citation(self, cluster, note_index):
        self.register_items()
        citation = { 'citationItems' : cluster,
                     'properties'    : { 'index'    : zotero_conn.get_index(cluster),
                                         'noteIndex': note_index } }
        res = self.methods.getCitationBlock(citation)
        return html2rst(unquote(res))

    def prefix_items(self, items):
        prefixed = {}
        for k in items.keys():
            v = items[k]
            prefixed["MY-%s"%(k)] = v
            v['id'] = "MY-%s"%(v['id'])
        return prefixed

    def load_biblio(self, path):
        self.local_items = json.load(open(path))
        self.methods.loadItems(self.prefix_items(self.local_items));
    
class ZoteroSetupDirective(Directive):
    def __init__(self, *args, **kwargs):
        Directive.__init__(self, *args)
        # This is necessary: connection hangs if created outside of an instantiated
        # directive class.
        if zot4rst.zotero_conn is None:
            zot4rst.zotero_conn = ZoteroConnection(self.options.get('format', DEFAULT_CITATION_FORMAT))
        else:
            zot4rst.zotero_conn.set_format(self.options.get('format', DEFAULT_CITATION_FORMAT))
        zot4rst.verbose_flag = self.state_machine.reporter.report_level

    required_arguments = 0
    optional_arguments = 0
    has_content = False
    option_spec = {'format' : directives.unchanged,
                   'keymap': directives.unchanged,
                   'biblio' : directives.unchanged }
    def run(self):
        if self.options.has_key('keymap'):
            zotero_conn.load_keymap(self.options['keymap'])
        if self.options.has_key('biblio'):
            zotero_conn.load_biblio(self.options['biblio'])
        return []

class ZoteroTransform(Transform):
    default_priority = 650
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
        cite_cluster = self.startnode.details['cite_cluster']
        # get the footnote label
        footnote_node = self.startnode.parent.parent
        note_index = 0
        if type(footnote_node) == nodes.footnote:
            note_index = int(str(footnote_node.children[0].children[0]))
        newnode = zotero_conn.get_citation(cite_cluster, note_index)
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
        pending = nodes.pending(ZoteroBibliographyTransform)
        pending.details.update(self.options)
        self.state_machine.document.note_pending(pending)
        return [pending]

class ZoteroBibliographyTransform(Transform):

    default_priority = 700

    def apply(self):
        newnode = zot4rst.zotero_conn.generate_rest_bibliography()
        self.startnode.replace_self(newnode)

def handle_cite_cluster(inliner, cite_cluster):
    def random_label():
        return "".join(random.choice(string.digits) for x in range(20))

    parent = inliner.parent
    document = inliner.document
    for cite in cite_cluster:
        cite.key = zotero_conn.lookup_key(cite.key)
        cite.id = zotero_conn.get_item_id(cite.key)
    zotero_conn.track_cluster(cite_cluster)
    if zotero_conn.in_text_style or \
            (type(parent) == nodes.footnote):
        # already in a footnote, or in-text style: just add a pending
        pending = nodes.pending(ZoteroTransform)
        pending.details['cite_cluster'] = cite_cluster
        document.note_pending(pending)
        return pending
    else:
        # not in a footnote & this is a footnote style; insert a
        # reference & add a footnote to the end

        label = random_label()

        refnode = nodes.footnote_reference('[%s]_' % label)
        refnode['auto'] = 1
        refnode['refname'] = label
        document.note_footnote_ref(refnode)
        document.note_autofootnote_ref(refnode)

        footnote = nodes.footnote("")
        footnote['auto'] = 1
        footnote['names'].append(label)
        pending = nodes.pending(ZoteroTransform)
        pending.details['cite_cluster'] = cite_cluster
        footnote += nodes.paragraph("", "", pending)
        document.note_pending(pending)
        document.note_autofootnote(footnote)
        where_to_add = parent
        while where_to_add is not None and \
                not(isinstance(where_to_add, nodes.Structural)):
            where_to_add = where_to_add.parent
        if where_to_add is None: where_to_add = document
        where_to_add += footnote
        return refnode

def zot_cite_role(role, rawtext, text, lineno, inliner,
                  options={}, content=[]):
    """Text role for citations."""
    check_zotero_conn()

    [first_cluster, second_cluster] = CiteParser().parse(text)
    # returns [citecluster, ...]
    nodeset = []
    if first_cluster is not None:
        nodeset.append(handle_cite_cluster(inliner, first_cluster))
        nodeset.append(nodes.Text(" ", rawsource=" "))
    nodeset.append(handle_cite_cluster(inliner, second_cluster))
    return nodeset, []

# setup zotero directives
directives.register_directive('zotero-setup', ZoteroSetupDirective)
directives.register_directive('zotero-bibliography', ZoteroBibliographyDirective)
roles.register_canonical_role('xcite', zot_cite_role)
