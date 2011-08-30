===================================
 ``zotero-plain`` example document
===================================

.. zotero-setup::
   :format: bluebook-19th

..
   :format: chicago-author-date

This is an example document which cites using footnotes [#]_ and
another [#]_ and another [#]_.

..
   And here we cite via ``zc``: :zc:`see @3A8VMRJ5, ch. 1`; :zc:`also @46SVHHG4`...

.. [#]
   :zc:`@3A8VMRJ5 851`
  
.. [#]
   :zc:`@46SVHHG4 11`

.. [#] I have taken the liberty of associating Posner's juxtaposition
   of the pyramids and hypertrophy in the anthropological sense with
   :zc:`@3A8VMRJ5`, in which the burial customs of the Pharaohs are
   given as an illustration of hypertrophy in the anthropological
   sense. :zc:`@46SVHHG4 851 $ suffix`
   
.. [#]
   Hello world.

..
   Bibliography
   ------------
   .. zotero-bibliography::
