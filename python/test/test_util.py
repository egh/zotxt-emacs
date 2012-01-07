# -*- coding: utf-8 -*-
from zot4rst.util import *
import unittest2

class TestUtil(unittest2.TestCase):
    def setUp(self):
        pass
    
    def test_unquote(self):
        self.assertEqual(["foo", "bar"], unquote(["foo", "bar"]))
        self.assertEqual(u"“Iñtërnâtiônàlizætiøn”", unicode(unquote("%u201CI%u00F1t%u00EBrn%u00E2ti%u00F4n%u00E0liz%u00E6ti%u00F8n%u201D")))
        self.assertEqual({"foo": "bar"}, unquote({"foo": "bar"}))
