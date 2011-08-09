# -*- coding: utf-8 -*-

'''Alternative strategy
    
Obtain output in target format direct from processor.

'''

from docutils import nodes
from docutils.parsers.rst import Directive
from docutils.parsers.rst import directives
from itertools import chain
import os, re, telnetlib, tempfile, time

import jsbridge

citation_format = "http://www.zotero.org/styles/bluebook-demo-x"

#def flatten(listOfLists):
#    return chain.from_iterable(listOfLists)

class Zotero(object):
    def __init__(self, **kwargs):
        global citation_format
        self.bibType = kwargs.get('bibType',
                                  citation_format)
        
        # setup bridge

    
    # This will be moved to the other side of the bridge, into the
    # plugin JS.
    def getItem(self, itemId):
        tmpfile = self.get_tmpfile_name()
        if not(re.match(r"^[0-9]+_", itemId)):
            itemId = "0_%s"%(itemId)
        self.cmd("var lkh = Zotero.Items.parseLibraryKeyHash(\"%s\");"%(itemId));
        self.cmd("var item = Zotero.Items.getByLibraryAndKey(lkh.libraryID, lkh.key);");
        self.cmd("var biblio = Zotero.QuickCopy.getContentFromItems(new Array(item), \"bibliography=%s\");"%(self.bibType))
        self.writeToFile(tmpfile, "biblio.html")
        while(not(os.path.exists(tmpfile))):
            time.sleep(0.1)
        html = open(tmpfile).read().decode('latin-1')
        retval = html2rst(html)
        print "ret of getItem: %s" % (type(retval),)
        return retval

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
