"""
  Module
"""
# -*- coding: utf-8 -*-
import docutils, docutils.parsers.rst, docutils.transforms, docutils.utils
import ConfigParser
import itertools
import jsbridge
import json
import os
import random
import re
import socket
import string
import sys
import zot4rst.jsonencoder

from zot4rst.util import html2rst, unquote
from xciterst.parser import CiteParser

DEFAULT_CITATION_FORMAT = "http://www.zotero.org/styles/chicago-author-date"

# placeholder for global bridge to Zotero
zotero_conn = None;

def check_zotero_conn():
    if not zot4rst.zotero_conn:
        ## A kludge, but makes a big noise about the extension syntax for clarity.
        sys.stderr.write("#####\n")
        sys.stderr.write("##\n")
        sys.stderr.write("##  Must set zotero-setup:: directive before zotero:: directive is used.\n")
        sys.stderr.write("##\n")
        sys.stderr.write("#####\n")
        raise docutils.utils.ExtensionOptionError("must set zotero-setup:: directive before zotero:: directive is used.")

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
        self.note_indexes = []
        self.citations = None

    def set_format(self, format):
        self.methods.instantiateCiteProc(format)

    def get_item_id(self, key):
        """Returns the id of an item with a given key. Key will be
        looked up in the local keymap before the id is looked up."""
        return self.get_item_id_batch([key])[0]

    def get_item_id_batch(self, keys):
        to_lookup = []
        for key in keys:
            if self.local_items.has_key(key):
                self.key2id[key] = "MY-%s"%(key)
            else:
                if not(self.key2id.has_key(key)):
                    to_lookup.append(key)
        if len(to_lookup) > 0:
            ids = json.loads(self.methods.getItemIdBatch(to_lookup))
            for n, new_id in enumerate(ids):
                self.key2id[to_lookup[n]] = new_id
        return [ self.key2id[key] for key in keys ]

    def load_keymap(self, path):
        self.keymap.read(os.path.relpath(path))

    def track_cluster(self, cluster):
        self.tracked_clusters.append(cluster)
        self.note_indexes.append(None)

    def register_items(self):
        def flatten(listoflists):
            return itertools.chain.from_iterable(listoflists)

        uniq_keys = set([ item.key for item in flatten(self.tracked_clusters) ])
        uniq_ids = set(self.get_item_id_batch(list(uniq_keys)))
        if (uniq_ids != self.registered_items):
            self.methods.updateItems(list(uniq_ids))
            self.registered_items = uniq_ids

    def get_index(self, cluster):
        return self.tracked_clusters.index(cluster)

    def generate_rest_bibliography(self):
        """Generate a bibliography of reST nodes."""
        self.register_items()
        data = self.methods.makeBibliography()
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

    def cache_citations(self):
        if (self.citations is None):
            self.register_items()
            citations = []
            for cluster in self.tracked_clusters: 
                index = self.get_index(cluster)
                citations.append({ 'citationItems' : cluster,
                                   'properties'    : { 'index'    : index,
                                                       'noteIndex': self.note_indexes[index] + 1} })
            for cit in citations:
                for c in cit['citationItems']:
                    c.id = self.get_item_id(c.key)
            # Implement mini-batching. This is a hack to avoid what
            # appears to be a string size limitation of some sort in
            # jsbridge or code that it calls.
            batchlen = 15
            offset = 0
            self.citations = []
            while len(self.citations) < len(citations):
                raw = self.methods.appendCitationClusterBatch(citations[offset:offset+batchlen])
                citation_blocks_html = json.loads(raw)
                self.citations.extend([ html2rst(unquote(block)) for block in citation_blocks_html ])
                offset = offset + batchlen
    def get_citation(self, cluster):
        self.cache_citations()
        return self.citations[self.get_index(cluster)]

    def prefix_items(self, items):
        prefixed = {}
        for k in items.keys():
            v = items[k]
            prefixed["MY-%s"%(k)] = v
            v['id'] = "MY-%s"%(v['id'])
        return prefixed

    def load_biblio(self, path):
        self.local_items = json.load(open(path))
        self.methods.registerLocalItems(self.prefix_items(self.local_items));
    
class ZoteroSetupDirective(docutils.parsers.rst.Directive):
    def __init__(self, *args, **kwargs):
        docutils.parsers.rst.Directive.__init__(self, *args)
        # This is necessary: connection hangs if created outside of an instantiated
        # directive class.
        if zot4rst.zotero_conn is None:
            zot4rst.zotero_conn = ZoteroConnection(self.options.get('format', DEFAULT_CITATION_FORMAT))
        else:
            zot4rst.zotero_conn.set_format(self.options.get('format', DEFAULT_CITATION_FORMAT))

    required_arguments = 0
    optional_arguments = 0
    has_content = False
    option_spec = {'format' : docutils.parsers.rst.directives.unchanged,
                   'keymap': docutils.parsers.rst.directives.unchanged,
                   'biblio' : docutils.parsers.rst.directives.unchanged }
    def run(self):
        if self.options.has_key('keymap'):
            zotero_conn.load_keymap(self.options['keymap'])
        if self.options.has_key('biblio'):
            zotero_conn.load_biblio(self.options['biblio'])
        pending = docutils.nodes.pending(ZoteroFootnoteSort)
        self.state_machine.document.note_pending(pending)
        return [pending]

class ZoteroFootnoteSort(docutils.transforms.Transform):
    default_priority = 641

    def apply(self):
        # Footnotes inserted via xcite are numbered before
        # normal reST auto-numbered footnotes, so we renumber
        # them as a single set, according to order of appearance
        # of the refs in text, taking care to keep the
        # ref and footnote numbering lined up.
        footnotemap = {}
        footnotes = self.document.autofootnotes
        for i in range(0, len(self.document.autofootnotes), 1):
            footnotemap[footnotes[i]['ids'][0]] = i
        newlist = []
        refs = self.document.autofootnote_refs
        for i in range(0, len(refs), 1):
            newlist.append(footnotes[footnotemap[refs[i]['refid']]])
        self.document.autofootnotes = newlist

        # The lists are now congruent and in document order, but the
        # footnote numbers are screwed up, and the notes themselves
        # may be in the wrong position.

        # Reassign numbers to the footnotes
        for i in range(0, len(self.document.autofootnotes), 1):
            label = self.document.autofootnotes[i].children[0]
            oldnum = label.children[0]
            newnum = docutils.nodes.Text(str(i + 1))
            label.replace(oldnum, newnum)

        # Move the footnotes themselves to a more sensible location
        # get the footnote label
        for i in range(0, len(self.document.autofootnotes), 1):
            footnote_node = self.document.autofootnotes[i]
            ref_node = self.document.autofootnote_refs[i]

            footnote_node.parent.remove(footnote_node)

            footnotes_at_end = getattr(self.document.settings, 'footnotes_at_end', 0)

            if footnotes_at_end:
                self.document += footnote_node
                self.document.setup_child(footnote_node)
            else:
                ref_parent = ref_node.parent
                ref_and_note = docutils.nodes.generated()
                ref_and_note += ref_node
                ref_and_note.setup_child(ref_node)
                ref_and_note += footnote_node
                ref_and_note.setup_child(footnote_node)
                ref_parent.replace(ref_node, ref_and_note)
                ref_parent.setup_child(ref_and_note)

        # Reassign numbers to the refs
        # (we don't touch these until now because they may contain
        # trojan footnotes)
        for i in range(0, len(self.document.autofootnote_refs), 1):
            ref = self.document.autofootnote_refs[i]
            if len(ref.children) == 2:
                ref.children.pop(0)
            oldnum = ref.children[0]
            newnum = docutils.nodes.Text(str(i + 1))
            ref.replace(oldnum, newnum)

        # Reset note numbers
        zotero_conn.citations = None
        zotero_conn.tracked_clusters = []
        zotero_conn.note_indexes = []
        for i in range(0, len(self.document.autofootnotes), 1):
            footnote = self.document.autofootnotes[i]
            content = footnote.children[1].children[0]
            if isinstance(footnote.children[1].children[0], docutils.nodes.pending):
            	cluster = content.details['cite_cluster']
		zotero_conn.tracked_clusters.append(cluster)
	        zotero_conn.note_indexes.append(i)
	    else:
		zotero_conn.tracked_clusters.append([])
	        zotero_conn.note_indexes.append(i)

        empty = docutils.nodes.generated()
        self.startnode.replace_self(empty)

class ZoteroCitationTransform(docutils.transforms.Transform):
    #
    # Before Footnote
    #
    default_priority = 538
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
        
        next_pending = docutils.nodes.pending(ZoteroCitationSecondTransform)
        next_pending.details['cite_cluster'] = cite_cluster
        self.document.note_pending(next_pending)
	self.startnode.replace_self(next_pending)

class ZoteroCitationSecondTransform(docutils.transforms.Transform):
    """Second pass transform for a Zotero citation. We use two passes
    because we want to generate all the citations in a batch, and we
    need to get the note indexes first."""
    #
    # After Footnote (to pick up the note number)
    #
    default_priority = 650
    def apply(self):
        cite_cluster = self.startnode.details['cite_cluster']
        footnote_node = self.startnode.parent.parent
        note_index = 0
        if type(footnote_node) == docutils.nodes.footnote:
            note_index = int(str(footnote_node.children[0].children[0]))
        zotero_conn.note_indexes[zotero_conn.get_index(cite_cluster)] = note_index
        cite_cluster = self.startnode.details['cite_cluster']
        newnode = zotero_conn.get_citation(cite_cluster)
        self.startnode.replace_self(newnode)

class ZoteroBibliographyDirective(docutils.parsers.rst.Directive):
    """Directive for bibliographies."""
    ## This could be extended to support selection of
    ## included bibliography entries. The processor has
    ## an API to support this, although it hasn't yet been
    ## implemented in any products that I know of.
    required_arguments = 0
    optional_arguments = 1
    has_content = False

    def run(self):
        pending = docutils.nodes.pending(ZoteroBibliographyTransform)
        pending.details.update(self.options)
        self.state_machine.document.note_pending(pending)
        return [pending]

class ZoteroBibliographyTransform(docutils.transforms.Transform):
    """Transform which generates a bibliography. Wait for all items to
    be registered, then we generate a bibliography."""
    default_priority = 700

    def apply(self):
        self.startnode.replace_self(zotero_conn.generate_rest_bibliography())

def handle_cite_cluster(inliner, cite_cluster):
    def random_label():
        return "".join(random.choice(string.digits) for x in range(20))

    parent = inliner.parent
    document = inliner.document
    for cite in cite_cluster:
        cite.key = zotero_conn.lookup_key(cite.key)
    zotero_conn.track_cluster(cite_cluster)
    if zotero_conn.in_text_style or \
            (type(parent) == docutils.nodes.footnote):
        # already in a footnote, or in-text style: just add a pending
        pending = docutils.nodes.pending(ZoteroCitationTransform)
        pending.details['cite_cluster'] = cite_cluster
        document.note_pending(pending)
        return pending
    else:
        # not in a footnote & this is a footnote style; insert a
        # reference & add a footnote to the end

        label = random_label()

	# Set up reference
        refnode = docutils.nodes.footnote_reference('[%s]_' % label)
        refnode['auto'] = 1
        refnode['refname'] = label
        document.note_footnote_ref(refnode)
        document.note_autofootnote_ref(refnode)

	# Set up footnote
        footnote = docutils.nodes.footnote("")
        footnote['auto'] = 1
        footnote['names'].append(label)
        pending = docutils.nodes.pending(ZoteroCitationTransform)
        pending.details['cite_cluster'] = cite_cluster
        paragraph = docutils.nodes.paragraph()
        paragraph.setup_child(pending)
        paragraph += pending
        footnote.setup_child(paragraph)
        footnote += paragraph
        document.note_pending(pending)
        document.note_autofootnote(footnote)
        
        # Temporarily stash footnote as a child of the refnode
        refnode.setup_child(footnote)
        refnode += footnote
        return refnode

def zot_cite_role(role, rawtext, text, lineno, inliner,
                  options={}, content=[]):
    """Text role for citations."""
    check_zotero_conn()

    [first_cluster, second_cluster] = CiteParser().parse(text)
    nodeset = []
    if first_cluster is not None:
        nodeset.append(handle_cite_cluster(inliner, first_cluster))
        nodeset.append(docutils.nodes.Text(" ", rawsource=" "))
    nodeset.append(handle_cite_cluster(inliner, second_cluster))
    return nodeset, []

class smallcaps(docutils.nodes.Inline, docutils.nodes.TextElement): pass

# setup zotero directives, roles
docutils.parsers.rst.directives.register_directive('zotero-setup', ZoteroSetupDirective)
docutils.parsers.rst.directives.register_directive('zotero-bibliography', ZoteroBibliographyDirective)
docutils.parsers.rst.roles.register_canonical_role('xcite', zot_cite_role)
docutils.parsers.rst.roles.register_local_role("smallcaps", smallcaps)
