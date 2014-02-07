=============
 zotxt-emacs
=============

Note: zotero-plain has been split into two parts: zotxt-emacs (this
repository) for working with org, and ``zot4rst`` (zotero
reStructuredText tools), which been moved to `zot4rst`_.

Emacs integration
-----------------

Emacs integration has 2 parts: a core library (``zotxt.el``),
`org-mode`_ integration (``org-zotxt.el``). Emacs integration depends
on the zotxt_ Zotero extension.

To install, add the following to your ``.emacs`` file::

  (add-to-list 'load-path "/path/to/zotxt-emacs/elisp/")
  (autoload 'org-zotxt-mode "org-zotxt" "" t)

For pandoc markdown files
-------------------------

zoxtxt-easykey, in combination with pandoc-zotxt, can help you edit
your pandoc markdown files. Load the zotxt-easykey minor mode using
``M-x zotxt-easykey-mode`` and get started by inserting an easykey
using ``C-c z k``. This will prompt you for a search string, which
will do a quicksearch in your Zotero library. You will then be
prompted to make a selection from the results of that search. An
easykey for your selection will then be inserted into your document.

zotxt + org-mode
~~~~~~~~~~~~~~~~

To insert a citation into a `org-mode`_ document, first enable the
``org-zotxt`` minor mode::

  M-x org-zotxt-mode

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

zotxt EasyKey integration
-------------------------

zotxt supports a feature known as “easy keys” for integration with
zotero and pandoc or zot4rst. These keys look like ``@doe:2014title``
where Doe is the author’s last name, 2014 is the publication year, and
title is the first word of the title (excluding “stop words”, such as
“the” or “a”). The provided ``zotxt-easykey-mode`` supports completion
of these easykeys in a buffer using the command
``completion-at-point``. (This function is not bound in org-mode, but
I replace the binding to ``pcomplete`` using::

  ;; prefer completion-at-point to pcomplete
  (define-key org-mode-map (kbd "C-M-i") 'completion-at-point)

Now, the user can ``@doe`` and then ``C-M-i``. If the only matching
easy key is ``@doe:2014title`` this will be completed. If there are
multiple matches, the user will be presented with a buffer containing
possible completions.
 

.. _Zotero: http://www.zotero.org/
.. _emacs: http://www.gnu.org/software/emacs/
.. _`org-mode`: http://orgmode.org/
.. _`zot4rst`: http://bitbucket.org/egh/zot4rst
.. _zotxt: http://bitbucket.org/egh/zotxt
