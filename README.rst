=========================================
 Zotero for plain text: ``zotero-plain``
=========================================

``zot4rst`` (zotero reStructuredText tools have been moved to
`http://bitbucket.org/egh/zot4rst`. This repository now contains
only some tools for emacs.

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

