# -*- coding: utf-8 -*-

'''Alternative strategy
    
Obtain output in target format direct from processor.

'''

from docutils import nodes
from docutils.parsers.rst import Directive
from docutils.parsers.rst import directives
from itertools import chain
import BeautifulSoup, os, re, telnetlib, tempfile, time

citation_format = "http://www.zotero.org/styles/bluebook-demo-x"


def flatten(listOfLists):
    return chain.from_iterable(listOfLists)

def html2rst(html):
    
    def cleanString(s):
        s = s.replace(u"\u001c", u"“")
        s = s.replace(u"\u001d", u"”")
        s = s.replace(u"\u0019", u"’")
        s = s.replace("&nbsp;", " ")
        s = s.replace("\n", "")
        return s

    def walk(node):
        if node == None:
            return nodes.Text("")
        elif ((type(node) == BeautifulSoup.NavigableString) or (type(node) == str) or (type(node) == unicode)):
            return nodes.Text(cleanString(unicode(node)))
        else:
            if (node.name == 'span'):
                if (node.has_key('style') and (node['style'] == "font-style:italic;")):
                    return nodes.emphasis(text="".join([ unicode(walk(c)) for c in node.contents ]))
                else:
                    return walk("".join([ str(c) for c in node.contents ]))
            if (node.name == 'i'):
                print node
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
    print "ret of html2rst: %s" % (type(doc),)
    return [ walk(c) for c in doc.contents ]

class Zotero(object):
    def cmd(self, cmd):
        self.t.write(("%s\n"%(cmd)).encode('latin-1'))
        retval = self.t.read_until("> ")
        return retval[0:-6]

    def __init__(self, **kwargs):
        global citation_format
        self.t = telnetlib.Telnet("localhost", 4242)
        self.t.read_until("> ")
        self.cmd("var Zotero = Components.classes[\"@zotero.org/Zotero;1\"] .getService(Components.interfaces.nsISupports).wrappedJSObject;")
        self.bibType = kwargs.get('bibType',
                                  citation_format)
        
    def get_tmpfile_name(self):
        tmpfile = tempfile.NamedTemporaryFile()
        tmpfile.close()
        if os.path.exists(tmpfile.name): os.remove(tmpfile.name)
        tmpfilename = tmpfile.name
        tmpfile = None
        return tmpfilename

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

    def writeToFile(self, filename, expr):
        self.cmd("""function writeFile(filename, data) {
  var file = Components.classes["@mozilla.org/file/local;1"].createInstance(Components.interfaces.nsILocalFile);
  var foStream = Components.classes["@mozilla.org/network/file-output-stream;1"].createInstance(Components.interfaces.nsIFileOutputStream);
  file.initWithPath(filename); 
  foStream.init(file, 0x02 | 0x08, 00666, 0);
  foStream.write(data, data.length);
  foStream.close();
};""")
        self.cmd("writeFile(\"%s\", %s);"%(filename, expr))

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

    def run(self):
        z = Zotero()
        ret = z.getItem(self.arguments[0])
        return ret
