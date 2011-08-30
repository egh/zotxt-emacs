#!/usr/bin/env python

try:
    from distutils.core import setup
except ImportError:
    from setuptools import setup

setup(name         = 'zotero4rst',
      version      = '0.1',
      description  = 'Zotero for reStructuredText',
      author       = 'Erik Hetzner',
      author_email = 'egh@e6h.org',
      url          = 'http://bitbucket.org/egh/zotero-plain/',
      packages     = ['zotero4rst'],
      scripts      = ['bin/zrst2html',
                      'bin/zrst2odt', 
                      'bin/zrst2pdf',
                      'bin/zrst2pseudoxml'],
      )
