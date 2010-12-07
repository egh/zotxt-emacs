from docutils import nodes
from docutils.parsers.rst import Directive
from docutils.parsers.rst import directives
from itertools import chain
import BeautifulSoup,re,telnetlib, tempfile

citation_format = "http://www.zotero.org/styles/chicago-note-bibliography"

def flatten(listOfLists):
    return chain.from_iterable(listOfLists)

def html2rst(html):
    def walk(node):
        if ((type(node) == BeautifulSoup.NavigableString) or (type(node) == str) or (type(node) == unicode)):
            return nodes.Text(unicode(node).replace("\n","").replace("&nbsp;",""))
        else:
            if (node.name == 'span'):
                if (node.has_key('style') and (node['style'] == "font-style:italic;")):
                    return nodes.emphasis(text="".join([ unicode(walk(c)) for c in node.contents ]))
                else:
                    return walk("".join([ str(c) for c in node.contents ]))
            elif (node.name == 'p'):
                children = [ walk(c) for c in node.contents ]
                return nodes.paragraph("", "", *children)
            elif (node.name == 'a'):
                children = [ walk(c) for c in node.contents ]
                return nodes.reference("", "", *children,  refuri=node['href'])
            elif (node.name == 'div'):
                return walk(node.p)

    doc = BeautifulSoup.BeautifulSoup(html)
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
        
    def getItem(self, itemId):
        tmpfile = tempfile.NamedTemporaryFile()
        if not(re.match(r"^[0-9]+_", itemId)):
            itemId = "0_%s"%(itemId)
        self.cmd("var lkh = Zotero.Items.parseLibraryKeyHash(\"%s\");"%(itemId));
        self.cmd("var item = Zotero.Items.getByLibraryAndKey(lkh.libraryID, lkh.key);");
        self.cmd("var biblio = Zotero.QuickCopy.getContentFromItems(new Array(item), \"bibliography=%s\");"%(self.bibType))
        self.writeToFile(tmpfile.name, "biblio.html")
        html = tmpfile.read().decode('latin-1')
        retval = html2rst(html)
        tmpfile.close()
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
    required_arguments = 1
    optional_arguments = 9999
    has_content = True

    def run(self):
        z = Zotero()
        return z.getItem(self.arguments[0])
