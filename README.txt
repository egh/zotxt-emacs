=========================================
 Zotero for plain text: ``zotero-plain``
=========================================

Background
----------

Zotero_ is a useful tool for managing citations.

``zotero-plain`` consists of two parts (so far): a tool for including
citations in reStructuredText_ documents, and a tool for interaction
with emacs_. Both tools use Zotero_ via mozrepl_ to generate citations
in various formats.

Installation
------------

1. Install Zotero_.
2. Install mozrepl_.
3. Start firefox. Using the Tools menu, start mozrepl_.

reStructuredText
----------------

Zotero_ citations may be added to a reStructuredText document using the
following syntax::

  .. zotero:: xxxxxxxx

When processing using Zotero_ aware ReST tools (see the ``rst2pdf``
and ``rst2html`` executables included in ``python/``), this directive
will be replaced with a properly formatted citation.

Where ``xxxxxxxx`` is the key as reported by Zotero_ in the “Generate
report from selected item” menu, or as in an item’s URI, e.g.::

  http://www.zotero.org/egh/items/xxxxxxxx

(See below for information about easier management using emacs.)

Anything after the first argument (the item key) is ignored by the
parser. This allows you to insert more descriptive information about
the item, e.g.::

  .. zotero:: xxxxxxxx Doe, John. (1999) A history.

It is recommended that this reStructuredText directive be combined
with the cite directive. For instance::

  Foo bar [Baz]_.

  .. [Baz]
    .. zotero:: xxxxxxxx

To include Zotero_ citations in a reStructuredText_ document, you must
use the bundled rst2pdf or rst2html scripts, which have been modified
to include support for ``zotero`` directives. These executables can be
found inside the ``python/`` directory in the source distribution.

Emacs integration
-----------------

Emacs integration has 3 parts: a core library (``zotero.el``),
`org-mode`_ integration (``org-zotero.el``), and reStructuredText_
integration (``zotero-rst.el``). Emacs integration depends on the
``moz.el`` file from the mozrepl_ project, which can be retrieved at
https://github.com/bard/mozrepl/blob/master/chrome/content/moz.el.

To install, add the following to your ``.emacs`` file::

  (add-to-list 'load-path "/path/to/moz.el")
  (add-to-list 'load-path "/path/to/zotero-plain/elisp/")
  (autoload 'zotero-rst-mode "zotero-rst" "" t)
  (autoload 'org-zotero-mode "org-zotero" "" t)
  
zotero + emacs + reStructuredText
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To insert a citation into a ReST document, first enable the
``zotero-rst`` minor mode::

  M-x zotero-rst-mode

Then select one or more items in a Zotero_ pane. Finally, in emacs_,
use: ``C-c z i`` to insert these items as citations.

To update a citation’s descriptive information from Zotero_ (remember,
rst2xxx tools will ignore this information, so you may do what you
wish with it), use ``C-c z u``.

zotero + org-mode
~~~~~~~~~~~~~~~~~

To insert a citation into a `org-mode`_ document, first enable the
``org-zotero`` minor mode::

  M-x org-zotero-mode

Then select one or more items ina Zotero_ pane. Finally, in emacs_,
use: ``C-c z i`` to insert these items as citations. This inserts a
zotero link with descriptive link text.

To update the link text to reflect changed metadata from Zotero_, use
``C-c z u`` over the link.

.. _Zotero: http://www.zotero.org/
.. _mozrepl: https://github.com/bard/mozrepl/wiki
.. _emacs: http://www.gnu.org/software/emacs/
.. _`org-mode`: http://orgmode.org/
.. _reStructuredText: http://docutils.sourceforge.net/rst.html
