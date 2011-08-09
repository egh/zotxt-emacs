# -*- coding: utf-8 -*-

from docutils import nodes
from docutils.parsers.rst import Directive
from docutils.parsers.rst import directives
from itertools import chain
import os, re, telnetlib, tempfile, time

import jsbridge

citation_format = "http://www.zotero.org/styles/chicago-author-date"
JSBRIDGE_PORT = 24242
JSBRIDGE_TIMEOUT = 60. # timeout for jsbridge

class Zotero(object):
    def __init__(self, **kwargs):
        global citation_format, JSBRIDGE_PORT, JSBRIDGE_TIMEOUT
        self.bibType = kwargs.get('bibType',
                                  citation_format)
        self.jsbridge_port = kwargs.get('jsbridge_port',
                                        JSBRIDGE_PORT)
        self.jsbridge_timeout = kwargs.get('jsbridge_timeout',
                                           JSBRIDGE_TIMEOUT)
        connection = self.firefox_connect()
        self.methods = self.zotero_resource()

    def firefox_connect(self):
        # get the bridge and the back-channel
        self.back_channel, self.bridge = jsbridge.wait_and_create_network("127.0.0.1",
                                                                          self.jsbridge_port)

        # set a timeout on jsbridge actions in order to ensure termination
        # (don't think we'll be needing back_channel)
        self.back_channel.timeout = self.bridge.timeout = self.jsbridge_timeout

    def zotero_resource(self):
        zotero = jsbridge.JSObject(self.bridge, "Components.utils.import('resource://zotero-for-restructured-text/modules/export.js')")
        return zotero
        
class ZoteroSetupDirective(Directive):
    from docutils.parsers.rst.directives import unchanged

    required_arguments = 0
    optional_arguments = 0
    has_content = False
    option_spec = {'format': unchanged}
    def run(self):
        global citation_format
        if self.options['format'] != "":
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
                   'suppress-author', directives.flag}

    def run(self):
        z = Zotero()
        
        ret = z.getItem(self.arguments[0])
        return ret
