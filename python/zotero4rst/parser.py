import zotero4rst

import sys
from pyparsing import Group, OneOrMore, Optional, Regex, Word, ZeroOrMore

class CiteParser(object):
    class Base():
        def __init__(self, name, content):
            self.content = content
            self.name = name

        def __str__(self):
            if type(self.content) == list:
                return "%s(%s)"%(self.name, ", ".join([ str(c) for c in self.content]))
            else: 
                return "%s(%s)"%(self.name, self.content)

    class Locator(Base):
        def __init__(self, content):
            CiteParser.Base.__init__(self, "Locator", content)

    class Suffix(Base):
        def __init__(self, content):
            CiteParser.Base.__init__(self, "Suffix", content)

    class Prefix(Base):
        def __init__(self, content):
            CiteParser.Base.__init__(self, "Prefix", content)

    class CiteKey(Base):
        def __init__(self, toks):
            self.suppress_author = False
            if len(toks) == 3:
                self.suppress_author = True
            self.key = toks[-1]
            CiteParser.Base.__init__(self, "CiteKey", self.key)

    class FullCite(Base):
        def __init__(self, toks):
            CiteParser.Base.__init__(self, "FullCite", toks.asList())

    class ShortCite(Base):
        def __init__(self, toks):
            self.suppress_author = False
            if len(toks) == 3:
                self.suppress_author = True
            self.key = toks[-1]
            CiteParser.Base.__init__(self, "ShortCite", self.key)

    class ShortCiteExtra(Base):
        def __init__(self, toks):
            CiteParser.Base.__init__(self, "ShortCiteExtra", toks.asList())

    def _results2cites(self, pieces, cites=[None, []], current_cite=None):
        prefix = None
        for piece in pieces:
            if isinstance(piece, CiteParser.ShortCite):
                # actually 2 cites, first author-only, then suppress-author
                first = zotero4rst.ZoteroCitationInfo(key=piece.key,
                                                      author_only=True)
                current_cite = zotero4rst.ZoteroCitationInfo(key=piece.key,
                                                             suppress_author=True)
                cites[0] = [first]
                cites[1].append(current_cite)
            elif isinstance(piece, CiteParser.CiteKey):
                current_cite = zotero4rst.ZoteroCitationInfo(key=piece.key,
                                                             suppress_author=piece.suppress_author,
                                                             prefix=prefix)
                cites[1].append(current_cite)
            elif isinstance(piece, CiteParser.Prefix):
                prefix = piece.content
            elif isinstance(piece, CiteParser.Locator):
                current_cite.locator = piece.content
            elif isinstance(piece, CiteParser.Suffix):
                current_cite.suffix = piece.content
            elif isinstance(piece, CiteParser.ShortCiteExtra):
                self._results2cites(piece.content, cites, current_cite)
            elif isinstance(piece, CiteParser.FullCite):
                self._results2cites(piece.content, cites)
        
    def parse(self, what):
        WORD_CHAR_RE = r'[\w.,-]'
        
        greedyToken = Regex(r'%s+'%(WORD_CHAR_RE))
        wordWithDigits = Regex(r'%s*[0-9]%s*'%(WORD_CHAR_RE, WORD_CHAR_RE))
        locator = OneOrMore(wordWithDigits) ^ (Optional(greedyToken) + OneOrMore(wordWithDigits))
        locator.setParseAction(lambda s,l,t: CiteParser.Locator(" ".join(t)))
        citeKey = Optional('-') + '@' + Regex(r'[\w-]+')
        citeKey.setParseAction(lambda s,l,t: CiteParser.CiteKey(t))
        suffix = OneOrMore(greedyToken)
        suffix.setParseAction(lambda s,l,t: CiteParser.Suffix(" ".join(t)))
        prefix = OneOrMore(greedyToken)
        prefix.setParseAction(lambda s,l,t: CiteParser.Prefix(" ".join(t)))

        # a short cite, author + (date)
        shortCite = Optional('-') + '@' + Regex(r'[\w-]+')
        shortCite.setParseAction(lambda s,l,t: CiteParser.ShortCite(t))

        # a full & complete cite (for use in brackets)
        fullCite = (citeKey | (prefix + citeKey)) + Optional(locator) + Optional(suffix)
        fullCite.setParseAction(lambda s,l,t: CiteParser.FullCite(t))

        restCite = ';' + fullCite

        bracketedCite = ('[' + fullCite + ZeroOrMore(restCite) + ']')

        shortCiteExtra = ('[' + locator + Optional(suffix) + ZeroOrMore(restCite) + ']')
        shortCiteExtra.setParseAction(lambda s,l,t: CiteParser.ShortCiteExtra(t))

        topCite = bracketedCite ^ shortCite + shortCiteExtra ^ shortCite + bracketedCite ^ shortCite

        raw = topCite.parseString(what, True)
        self._results2cites(list(raw))
        return cites
