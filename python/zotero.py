"""
  Module
"""
# -*- coding: utf-8 -*-

from jsbridge import wait_and_create_network, JSObject

from docutils import nodes
from docutils.parsers.rst import Directive
from docutils.parsers.rst import directives
from itertools import chain
import os, re, time
from docutils.transforms import TransformError, Transform


citation_format = "http://www.zotero.org/styles/chicago-author-date"
item_list = []
item_array = {}
zotero_thing = None;

class ZoteroConnection(object):
    def __init__(self, **kwargs):
        self.bibType = kwargs.get('bibType',
                                  citation_format)
        self.firefox_connect()
        self.zotero_resource()

    def firefox_connect(self):
        # get the bridge and the back-channel
        self.back_channel, self.bridge = wait_and_create_network("127.0.0.1",
                                                                          24247)

        self.back_channel.timeout = self.bridge.timeout = 60

    def zotero_resource(self):
        self.methods = JSObject(self.bridge, "Components.utils.import('resource://zotero-for-restructured-text/modules/export.js')")

class ZoteroSetupTransformDirective(Transform):
    default_priority = 500
    def apply(self):
        global zotero_thing
        # Do stuff before trashing the node
        print "================ Setup run #2 (load IDs to processor) =============="
        ##self.state_machine.document['zotero_connection'].reStructuredCSL.updateItems(item_list)
        zotero_thing.registerItemIds(item_list)
        self.startnode.parent.remove(self.startnode)
        # Do stuff after trashing the node
    

class ZoteroSetupDirective(Directive, ZoteroConnection):

    from docutils.parsers.rst.directives import unchanged

    def __init__(self, one, two, three, four, five, six, seven, eight, nine):
        global zotero_thing
        # There MUST be a better way to do this.
        Directive.__init__(self, one, two, three, four, five, six, seven, eight, nine)
        # This is necessary: connection hangs if created outside of an instantiated
        # directive class.
        ZoteroConnection.__init__(self)

        ##self.state_machine.document['zotero_connection'] = self.methods
        zotero_thing = self.methods
        #print 'Register on state machine: %s' % self.state_machine.document['zotero_connection']

    required_arguments = 0
    optional_arguments = 0
    has_content = False
    option_spec = {'format': unchanged}
    def run(self):
        global citation_format, zotero_thing
        print "================ Setup run #1 (establish connection, spin up processor) =============="
        if self.options.has_key('format'):
            citation_format = self.options['format']
        ##self.state_machine.document['zotero_connection'].instantiateCiteProc(citation_format);
        zotero_thing.instantiateCiteProc(citation_format);
        pending = nodes.pending(ZoteroSetupTransformDirective)
        pending.details.update(self.options)
        self.state_machine.document.note_pending(pending)
        return [pending]

class ZoteroTransformDirective(Transform):
    default_priority = 510
    def apply(self):
        # Do stuff before trashing the node
        print "================ Citation run #2 (render cite and insert) ================"
        print zotero_thing.getCitationBlock({'citationItems':[{'id':66}],'properties':{'noteIndex':1}})
        self.startnode.parent.remove(self.startnode)
        # Do stuff after trashing the node
        

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
        global citation_format, item_list, item_array, zotero_thing
        #print '  Found on state machine: %s' % self.state_machine.document['zotero_connection']
        #print 'Argh! %s' % self.arguments[0]
        # Arrange to complain if zotero_setup:: declaration was missing.
        print "================ Citation run #1 (record ID) ================"
        ##itemID = int(self.state_machine.document['zotero_connection'].getItemId(self.arguments[0]))
        itemID = int(zotero_thing.getItemId(self.arguments[0]))
        if not item_array.has_key(itemID):
            item_array[itemID] = True
            item_list.append(itemID)
        # Probably no need to stash anything in the options, but there they are.
        #print self.options
        pending = nodes.pending(ZoteroTransformDirective)
        pending.details.update(self.options)
        self.state_machine.document.note_pending(pending)
        return [pending]
