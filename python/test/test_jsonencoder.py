from zot4rst.jsonencoder import ZoteroJSONEncoder
from xciterst import CitationCluster, CitationInfo
import unittest2

class TestJsonencoder(unittest2.TestCase):
    def setUp(self):
        self.encoder = ZoteroJSONEncoder()
        self.citation = CitationInfo(key="foo")
        self.citation.id = "foo"
        citation2 = CitationInfo(key="bar")
        citation2.id = "bar"
        self.citation_cluster = CitationCluster([self.citation, citation2])

    def test_basic(self):
        json = self.encoder.encode(self.citation)
        self.assertEqual(json, '{"id": "foo"}')

    def test_suppress_author(self):
        self.citation.suppress_author = True
        json = self.encoder.encode(self.citation)
        self.assertEqual(json, '{"suppress-author": true, "id": "foo"}')

    def test_author_only(self):
        self.citation.author_only = True
        json = self.encoder.encode(self.citation)
        self.assertEqual(json, '{"id": "foo", "author-only": true}')
        
    def test_all(self):
        self.citation.prefix = "see"
        self.citation.suffix = "and nowhere else",
        self.citation.locator = "p. 10"
        json = self.encoder.encode(self.citation)
        self.assertEqual(json, '{"locator": "p. 10", "prefix": "see ", "id": "foo", "suffix": " and nowhere else"}')

    def test_cluster(self):
        self.citation_cluster.index = 2
        self.citation_cluster.note_index = 3
        json = self.encoder.encode(self.citation_cluster)
        self.assertEqual(json, '{"citationItems": [{"id": "foo"}, {"id": "bar"}], "properties": {"index": 2, "noteIndex": 3}}')
