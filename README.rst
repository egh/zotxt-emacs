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

zot4rst
-------

Installation
~~~~~~~~~~~~

1. Install Zotero_.
2. Install docutils from a `docutils snapshot`_ release.
3. Install zot4rst (from the ``python`` directory::

     sudo python setup.py install

4. Build the Firefox extension (from the ``extension`` directory::

     sh build.sh

5. Install the extension built (``zotero-for-restructured-text.xpi``)

Quickstart
~~~~~~~~~~

See ``example/example.rst``, and the generated ``example/example.pdf``
and ``example/example.html``. Citation syntax is identical to pandoc.

For the time being zot4rst depends on the rather obscure zotero
library key (which look like, e.g., ``0_MRCENTE5``). To make this
work, we use a mapping file to map between a human-readable key and
the Zotero key. This will map, for instance, from ``Doe2011`` to
``0_MRCENTE5``. See ``example/example.keys`` for an example mapping
between the human-readable key and the zotero library key.

You can use the bundled ``zupdatekeymap`` script to generate this
keymap file from an existing Zotero collection. To do this, you need
to an API key, available at http://www.zotero.org/settings/keys/new

After you have your userid and key, run::

  zupdatekeymap -u USERID -k KEY KEYMAPFILE

where ``USERID`` is your *numeric* userid, ``KEY`` is your API key,
and ``KEYMAPFILE`` is the file you want to use to hold a mapping from
human keys to Zotero keys. The script will prompt you for a collection
to use; select one. The files should then be generated.

You can then edit the file, choosing keys that are more to your
liking.

After generating this mapping file, you can run::

  zupdatekeymap KEYMAPFILE

to keep it up to date with any new items in the collection. Your API
key is stored in the file, so if it is a write key, you will not want
to distribute this key file.

Updating the keymap should *not* change any keys you have modified by
hand, but a backup is always made then ``zupdatekeymap`` is run.

To include Zotero_ citations in a reStructuredText_ document, you must
use the bundled ``zrst2*`` scripts, which have been modified to
include support for ``zotero`` directives. These executables are
installed using ``setup.py`` above. Currently, they are:

- ``zrst2html``
- ``zrst2odt``
- ``zrst2pdf``
- ``zrst2pseudoxml``
- ``zrst2rst``

Portable operation
~~~~~~~~~~~~~~~~~~

zot4rst has a second mode can be used to process citations independent
of a user’s library. This means that you can use zot4rst without
having access to the original Zotero library.

To use this feature, run::

  zupdatekeymap KEYMAPFILE JSONFILE

For example:

  zupdatekeymap example.keys example.json

This will save your citations in the ``example.json`` file. You will
know want to change the ``keymap`` option to ``biblio`` and point at
the ``example.json`` file (see ``example_portable.rst``)

Now you can distribute your ``.json`` file alongside your RST file,
and others who have installed zot4rst can process it, without needing
your library.

You can try this out yourself by running::

  zrst2pdf example_portable.rst

This should work successfully, generating citations and a bibliography
for items that are not in your Zotero library.

Details
~~~~~~~

Some details, in no particular order.

Note that ``zrst2rst`` will transform your citations into plain
reStructuredText files without the Zotero extension. For example::

  A citation group :xcite:`[see @item1 p. 34-35; also @item3 chap. 3]`.

will become::

  A citation group (see Doe 2005, p. 34–35; also Doe and Roe 2007, chap. 3).

and the bibliography will be fully expanded. This can be used to
create RST files that will work without zot4st.

If you use a footnote citation format, zot4rst will insert footnotes
for you.

However, if you also use regular autonumbered footnotes in the same
section or paragraph, the ordering will be wrong. So if you want to do
this, you will need to put your citations in a footnote
explicitly. For example::

  Water is wet. [#]_ But there are those who dispute it. [#]_

  .. [#] :xcite:`[See @item3]`.

  .. [#] These people are wrong.

Emacs integration
-----------------

Emacs integration has 2 parts: a core library (``zotero.el``),
`org-mode`_ integration (``org-zotero.el``). Emacs integration depends
on the ``moz.el`` file from the mozrepl_ project, which can be
retrieved at
https://github.com/bard/mozrepl/blob/master/chrome/content/moz.el.

To install, add the following to your ``.emacs`` file::

  (add-to-list 'load-path "/path/to/moz.el")
  (add-to-list 'load-path "/path/to/zotero-plain/elisp/")
  (autoload 'org-zotero-mode "org-zotero" "" t)

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
.. _`docutils snapshot`: http://docutils.sourceforge.net/docutils-snapshot.tgz
