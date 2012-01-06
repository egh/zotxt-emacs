#!/usr/bin/env python
from setuptools import setup

setup(name             = 'zot4rst',
      version          = '0.2dev',
      description      = 'Zotero for reStructuredText',
      author           = 'Erik Hetzner',
      author_email     = 'egh@e6h.org',
      url              = 'http://bitbucket.org/egh/zotero-plain/',
      packages         = ['zot4rst',
                          'xciterst'],
      install_requires = ["BeautifulSoup>=3.2.0",
                          "docutils>=0.9",
                          "rst2pdf>=0.16",
                          "jsbridge>=2.4.7",
                          "pyparsing>=1.5.6",
                          "pyzotero>=0.8.8"],
      scripts          = ['bin/zotcite',
                          'bin/zrst2html',
                          'bin/zrst2odt', 
                          'bin/zrst2pdf',
                          'bin/zrst2pseudoxml',
                          'bin/zrst2rst',
                          'bin/zupdatekeymap']
      )
