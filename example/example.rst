docutils with Zotero
====================

Shamelessly stolen from the pandoc example:
http://johnmacfarlane.net/pandoc/demo/CITATIONS

.. zotero-setup::
   :keymap: example.keys
   :format: chicago-author-date

.. -   :zc:`[@nonexistent]`

.. -   :zc:`@nonexistent`

- :zc:`@item1` says blah.

- :zc:`@item1 [p. 30]` says blah.

- :zc:`@item1 [p. 30] with suffix` says blah.

- :zc:`@item1; -@item2 [p. 30]; see also @item3` says blah.

- In a note. [#]_

- A citation group :zc:`see @item1 [p. 34-35]; also @item3 [chap. 3]`.

- Another one :zc:`see @item1 [p. 34-35]`.

- And another one in a note. [#]_

- Citation with a suffix and locator :zc:`@item1 [pp. 33, 35-37] and nowhere else`.

- Citation with suffix only :zc:`@item1 and nowhere else`.

- Now some modifiers. [#]_

.. - With some markup :zc:`*see* @item1 p. **32**`.

.. [#]
   A citation without locators :zc:`@item3`.

.. [#]
   Some citations :zc:`see @item2 [chap. 3]; @item3; @item1`.

.. [#]
   Like a citation without author: :zc:`-@item1`, and now Doe with a locator :zc:`-@item2 [p. 44]`.

References
==========
.. zotero-bibliography::
