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


citation_format = "http://www.zotero.org/styles/chicago-author-date"
item_list = []
item_array = {}
processor_ready = False

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

class ZoteroSetupDirective(Directive, ZoteroConnection):

    from docutils.parsers.rst.directives import unchanged

    def __init__(self, one, two, three, four, five, six, seven, eight, nine):
        # There MUST be a better way to do this.
        Directive.__init__(self, one, two, three, four, five, six, seven, eight, nine)
        # This is necessary: connection hangs if created outside of an instantiated
        # directive class.
        ZoteroConnection.__init__(self)

        self.state_machine.document['zotero_connection'] = self.methods
        #print 'Register on state machine: %s' % self.state_machine.document['zotero_connection']

    required_arguments = 0
    optional_arguments = 0
    has_content = False
    option_spec = {'format': unchanged}
    def run(self):
        global citation_format
        if self.options.has_key('format'):
            citation_format = self.options['format']
        return []

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
        global citation_format, item_list, item_array, processor_ready
        #print '  Found on state machine: %s' % self.state_machine.document['zotero_connection']
        #print 'Argh! %s' % self.arguments[0]
        # Arrange to complain if zotero_setup:: declaration was missing.
        itemID = int(self.state_machine.document['zotero_connection'].getItemId(self.arguments[0]))
        if not item_array.has_key(itemID):
            item_array[itemID] = True
            item_list.append(itemID)
        print item_list
        return []
