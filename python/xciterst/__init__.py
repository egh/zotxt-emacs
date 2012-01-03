import re

class CitationInfo(object):
    """Class to hold information about a citation for passing to
    citeproc."""

    def __init__(self, **kwargs):
        self.key = kwargs.get('key')
        self.label = kwargs.get('label', None)
        self.locator = kwargs.get('locator', None)
        self.suppress_author = kwargs.get('suppress_author', False)
        self.prefix = kwargs.get('prefix', None)
        if self.prefix:
            self.prefix = re.sub(r'\s+,', ',', self.prefix)
        self.suffix = kwargs.get('suffix', None)
        if self.suffix:
            self.suffix = re.sub(r'\s+,', ',', self.suffix)
        self.author_only = kwargs.get('author_only', False)
        self.id = None
    
    def __str__(self):
        return "%s %s(%s) %s"%(self.prefix, self.key, self.locator, self.suffix)

    def __eq__(self, other):
        return ((self.key == other.key) and
                (self.label == other.label) and
                (self.locator == other.locator) and
                (self.suppress_author == other.suppress_author) and
                (self.prefix == other.prefix) and
                (self.suffix == other.suffix) and
                (self.author_only == other.author_only) and
                (self.id == other.id))
                
class CitationCluster(object):
    """Class to hold a cluster of citations, with information about
    them suitable for submission to citeproc."""

    def __init__(self, citations):
        self.citations = citations
        self.note_index = 0
        self.index = 0

    def __eq__(self, other):
        return ((self.citations == other.citations) and
                (self.note_index == other.note_index) and
                (self.index == other.index))
