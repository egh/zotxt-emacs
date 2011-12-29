from xciterst.parser import CiteParser
from xciterst import CitationInfo

import unittest2

class TestXciteParserp(unittest2.TestCase):
    def setUp(self):
        self.parser = CiteParser()

    def parse(self, parse_string):
        return self.parser.parse(parse_string)
        
    # @item1
    def test_parse_1(self):
        [first_cluster, second_cluster] = self.parse("@item1")
        self.assertEqual(first_cluster, 
                         [CitationInfo(key="item1", author_only=True)])
        self.assertEqual(second_cluster, 
                         [CitationInfo(key="item1", suppress_author=True)])

    # @item1, [p. 30]
    def test_parse_2(self):
        [first_cluster, second_cluster] = self.parse("@item1 [p. 30]")
        self.assertEqual(first_cluster,
                         [CitationInfo(key="item1", author_only=True)])
        self.assertEqual(second_cluster,
                         [CitationInfo(key="item1", suppress_author=True, locator="p. 30")])

    # @item1 [p. 30, with suffix]
    # XXX - is this parse right?
    def test_parse_3(self):
        [first_cluster, second_cluster] = self.parse("@item1 [p. 30, with suffix]")
        self.assertEqual(first_cluster,
                         [CitationInfo(key="item1", author_only=True)])
        self.assertEqual(second_cluster,
                         [CitationInfo(key="item1", 
                                       suppress_author=True, 
                                       locator="p. 30,",
                                       suffix="with suffix")])
                         
    # @item1 [-@item2 p. 30; see also @item3]
    def test_parse_4(self):
        [first_cluster, second_cluster] = self.parse("@item1 [-@item2 p. 30; see also @item3]")
        self.assertEqual(first_cluster,
                         [CitationInfo(key="item1", author_only=True)])
        self.assertEqual(second_cluster,
                         [CitationInfo(key="item1",
                                       suppress_author=True),
                          CitationInfo(key="item2",
                                       suppress_author=True,
                                       locator="p. 30"),
                          CitationInfo(key="item3",
                                       prefix="see also")])

    # [see @item1 p. 34-35; also @item3 chap. 3]
    def test_parse_5(self):
        [first_cluster, second_cluster] = self.parse("[see @item1 p. 34-35; also @item3 chap. 3]")
        self.assertEqual(first_cluster, None)
        self.assertEqual(second_cluster,
                         [CitationInfo(key="item1",
                                       prefix="see",
                                       locator="p. 34-35"),
                          CitationInfo(key="item3",
                                       prefix="also",
                                       locator="chap. 3")])

    # [see @item1 p. 34-35]
    def test_parse_6(self):
        [first_cluster, second_cluster] = self.parse("[see @item1 p. 34-35]")
        self.assertEqual(first_cluster, None)
        self.assertEqual(second_cluster,
                         [CitationInfo(key="item1",
                                       prefix="see",
                                       locator="p. 34-35")])

    # [@item1 pp. 33, 35-37 and nowhere else]
    def test_parse_7(self):
        [first_cluster, second_cluster] = self.parse("[@item1 pp. 33, 35-37 and nowhere else]")
        self.assertEqual(first_cluster, None)
        self.assertEqual(second_cluster,
                         [CitationInfo(key="item1",
                                       locator="pp. 33, 35-37",
                                       suffix="and nowhere else")])
        
    # [@item1 and nowhere else]
    def test_parse_8(self):
        [first_cluster, second_cluster] = self.parse("[@item1 and nowhere else]")
        self.assertEqual(first_cluster, None)
        self.assertEqual(second_cluster,
                         [CitationInfo(key="item1",
                                       suffix="and nowhere else")])

    # XXX BROKEN
    # [*see* @item1 p. **32**]
    # def test_parse_9(self):
    #    [first_cluster, second_cluster] = self.parse("[*see* @item1 p. **32**]")
    #    self.assertEqual(first_cluster, None)
    #    self.assertEqual(second_cluster,
    #                     [CitationInfo(key="item1",
    #                                   prefix="<i>see</i>",
    #                                   locator="p. <b>32</b>")])
        
    # [@item3]
    def test_parse_10(self):
        [first_cluster, second_cluster] = self.parse("[@item3]")
        self.assertEqual(first_cluster, None)
        self.assertEqual(second_cluster,
                         [CitationInfo(key="item3")])

    # [see @item2 chap. 3; @item3; @item1]
    def test_parse_11(self):
        [first_cluster, second_cluster] = self.parse("[see @item2 chap. 3; @item3; @item1]")
        self.assertEqual(first_cluster, None)
        self.assertEqual(second_cluster,
                         [CitationInfo(key="item2",
                                       prefix="see",
                                       locator="chap. 3"),
                          CitationInfo(key="item3"),
                          CitationInfo(key="item1")])

    # [-@item1]
    def test_parse_12(self):
        [first_cluster, second_cluster] = self.parse("[-@item1]")
        self.assertEqual(first_cluster, None)
        self.assertEqual(second_cluster,
                         [CitationInfo(key="item1",
                                       suppress_author=True)])

    # [-@item2 p. 44]
    def test_parse_13(self):
        [first_cluster, second_cluster] = self.parse("[-@item2 p. 44]")
        self.assertEqual(first_cluster, None)
        self.assertEqual(second_cluster,
                         [CitationInfo(key="item2",
                                       suppress_author=True,
                                       locator="p. 44")])

    # [@item1, p. 30]
    def test_parse_14(self):
        [first_cluster, second_cluster] = self.parse("[@item1, p. 30]")
        self.assertEqual(first_cluster, None)
        self.assertEqual(second_cluster,
                         [CitationInfo(key="item1",
                                       locator="p. 30")])

if __name__ == '__main__':
    unittest2.main()
