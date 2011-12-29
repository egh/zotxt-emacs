from zot4rst.jsonencoder import ZoteroJSONEncoder
from xciterst import CitationInfo
import unittest2

class TestJsonencoder(unittest2.TestCase):
    def setUp(self):
        self.encoder = ZoteroJSONEncoder()
        self.citation = CitationInfo(key="foo")
        self.citation.id = "foo"
        
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
