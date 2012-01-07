import BeautifulSoup
import jsbridge
import re
import urllib
import zot4rst

from docutils import nodes

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
            # whitespace is significant in reST, so normalize empties to a single space
            if re.match("^\s+$", text):
                return nodes.Text(" ", rawsource=" ")
            else:
                return nodes.Text(text, rawsource=text)
        else:
            if (html_node.name == 'span'):
                if (html_node.has_key('style') and (html_node['style'] == "font-style:italic;")):
                    return nodes.emphasis(text="".join([ unicode(walk(c)) for c in html_node.contents ]))
                elif (html_node.has_key('style') and (html_node['style'] == "font-variant:small-caps;")):
                    return zot4rst.smallcaps(text="".join([ unicode(walk(c)) for c in html_node.contents ]))
                else:
                    return compact(walk("".join([ str(c) for c in html_node.contents ])))
            if (html_node.name == 'i'):
                return nodes.emphasis(text="".join([ unicode(walk(c)) for c in html_node.contents ]))
            elif (html_node.name == 'b'):
                return nodes.strong(text="".join([ unicode(walk(c)) for c in html_node.contents ]))
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
        res = urllib.unquote(unicode(source))
        if '%u' in res:
            reslst = re.split(r'(%u[A-Za-z0-9]{4})', res)
            for i in range(1, len(reslst), 2):
                reslst[i] = reslst[i].replace('%u','\\u').decode('unicode_escape')
            res = u"".join(reslst)
        return res
    else:
        return source
