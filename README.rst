=========
 zotmacs
=========

Note: zotero-plain has been split into two parts: zotmacs (this
repository) for working with org, and ``zot4rst`` (zotero
reStructuredText tools), which been moved to `zot4rst`_.

Emacs integration
-----------------

Emacs integration has 2 parts: a core library (``zotero.el``),
`org-mode`_ integration (``org-zotero.el``). Emacs integration depends
on the zotxt_ Zotero extension.

To install, add the following to your ``.emacs`` file::

  (add-to-list 'load-path "/path/to/zotmacs/elisp/")
  (autoload 'org-zotero-mode "org-zotero" "" t)

zotero + org-mode
~~~~~~~~~~~~~~~~~

To insert a citation into a `org-mode`_ document, first enable the
``org-zotero`` minor mode::

  M-x org-zotero-mode

Then select one or more items in Zotero_ pane. Finally, in emacs_,
use: ``C-c z i`` to insert these items as citations. This inserts a
zotero link with descriptive link text.

To update the link text to reflect changed metadata from Zotero_, use
``C-c z u`` over the link.

Following links
---------------

To ensure that ``zotero:`` links are followed by xdg-open, the default
browser called by emacs, you may need to run the following command::
  
  xdg-mime default firefox.desktop x-scheme-handler/zotero

.. _Zotero: http://www.zotero.org/
.. _emacs: http://www.gnu.org/software/emacs/
.. _`org-mode`: http://orgmode.org/
.. _`zot4rst`: http://bitbucket.org/egh/zot4rst
.. _zotxt: http://bitbucket.org/egh/zotxt
