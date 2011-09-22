docutils with Zotero
====================

Shamelessly stolen from the pandoc example:
http://johnmacfarlane.net/pandoc/demo/CITATIONS

.. zotero-setup::
   :keymap: example.keys
   :format: chicago-author-date

.. -   :xcite:`[@nonexistent]`

.. -   :xcite:`@nonexistent`

- :xcite:`@item1` says blah.

- :xcite:`@item1 [p. 30]` says blah.

- :xcite:`@item1 [p. 30, with suffix]` says blah.

- :xcite:`@item1 [-@item2 p. 30; see also @item3]` says blah.

- In a note.\ [#]_

- A citation group :xcite:`[see @item1 p. 34-35; also @item3 chap. 3]`.

- Another one :xcite:`[see @item1 p. 34-35]`.

- And another one in a note.\ [#]_

- Citation with a suffix and locator :xcite:`[@item1 pp. 33, 35-37 and nowhere else]`.

- Citation with suffix only :xcite:`[@item1 and nowhere else]`.

- Now some modifiers.\ [#]_

- With some markup :xcite:`[*see* @item1 p. **32**]`.

.. [#] A citation without locators :xcite:`[@item3]`.

.. [#] Some citations :xcite:`[see @item2 chap. 3; @item3; @item1]`.

.. [#] Like a citation without author: :xcite:`[-@item1]`, and now Doe
   with a locator :xcite:`[-@item2 p. 44]`.

References
==========
.. zotero-bibliography::
