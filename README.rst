=========================================
 Zotero for plain text: ``zotero-plain``
=========================================

Background
----------

Zotero_ is a useful tool for managing citations.

``zotero-plain`` consists of two parts: an extension to the Python
docutils_ package for including citations in reStructuredText_
documents (zotero2rst), and elisp code for accessing Zotero from
emacs_.

zotrero4st
----------

Installation
~~~~~~~~~~~~

1. Install Zotero_.
2. Install zotero4rst (from the ``python`` directory::

     sudo python setup.py install

3. Build the Firefox extension (from the ``extension`` directory::

     sh build.sh

4. Install the extension built (``zotero-for-restructured-text.xpi``)

Quickstart
~~~~~~~~~~

See ``example/example.rst``, and the generated ``example/example.pdf``
and ``example/example.html``. Citation syntax is identical to pandoc.

For the time being (...)  zotero4rst depends on the rather obscure
zotero library key. See ``example/example.keys`` for the mapping
between the human-readable key and the zotero library key.

There are at least two ways to determine the library key for an item
in your own collection. The first is to right click on an item, and
choose “Generate report from selected item”. You will then be visiting
a page whose URL resembles::

  zotero://report/items/0_MRCENTE5/html/report.html

In this URL, the string ``MRCENTE5`` is the library key. Use this
in your ``.keys`` mapping file.

The other way is to view an item on zotero.org, in this case the URL
will look like::

  http://www.zotero.org/egh/items/MRCENTE5

where the key is again ``MRCENTE5``.

(See below for information about easier management using emacs.)

To include Zotero_ citations in a reStructuredText_ document, you must
use the bundled ``zrst2*`` scripts, which have been modified to
include support for ``zotero`` directives. These executables are
installed using ``setup.py`` above. Currently, they are:

- ``zrst2html``
- ``zrst2odt``
- ``zrst2pdf``
- ``zrst2pseudoxml``

Details
~~~~~~~

Some details, in no particular order.

If you use a footnote citation format, zotero4rst will insert
footnotes for you.

However, if you also use regular autonumbered footnotes in the same
section or paragraph, the ordering will be wrong. So if you want to do
this, you will need to put your citations in a footnote
explicitly. For example::

  Water is wet. [#]_ But there are those who dispute it. [#]_

  .. [#] :xcite:`[See @item3]`.

  .. [#] These people are wrong.

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
.. _docutils: http://docutils.sourceforge.net/

