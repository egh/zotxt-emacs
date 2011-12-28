class CitationInfo(object):
    """Class to hold information about a citation for passing to
    citeproc."""

    def __init__(self, **kwargs):
        self.key = kwargs.get('key')
        self.label = kwargs.get('label', None)
        self.locator = kwargs.get('locator', None)
        self.suppress_author = kwargs.get('suppress_author', False)
        self.prefix = kwargs.get('prefix', None)
        self.suffix = kwargs.get('suffix', None)
        self.author_only = kwargs.get('author_only', False)
        self.id = None

class CitationCluster(object):
    """Class to hold a cluster of citations, with information about
    them suitable for submission to citeproc."""

    def __init__(self, citations):
        self.citations = citations
        self.note_index = 0
        self.index = 0
