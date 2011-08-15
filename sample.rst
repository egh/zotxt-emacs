==================================
 ``zotero-plain`` example document
==================================

.. zotero-setup::
   :format: chicago-author-date

This is an example [#]_ document which cites one source. [#]_ And another source. [#]_ And another. [#]_
With a final one here. [#]_

.. [#] Zotero directives should always be placed inside a note. Author-date styles will
   (eventually) be adjusted automagically during ReST processor. In note styles,
   zotero directives will be merged to a preceding paragraph. The same source serves for
   all citation style transformations.

   .. zotero:: DDW6JB2S


.. [#] 
   .. zotero:: DDW6JB2S   Brooker
      :prefix: Affixes can be added to a cite as an option. For special formatting
               in cite affixes, use the ersatz-HTML markup recognized by the CSL processor. 
               <i>See, e.g. </i> 


.. [#] 
   .. zotero:: DDW6JB2S   Brooker

   .. zotero:: DDW6JB2S
      :suffix: <i> </i>(adjacent zotero cites are merged into a single multiple citation;
               enclose leading or trailing space on affixes in markup to force its preservation)

.. [#] 
   .. zotero:: MAJBZZZ4   Swan, Michael. Practical English Usage. 3rd ed. Oxford University Press, USA, 2005.
      :suffix: <i> </i>(note that <sc>Small Caps</sc> are available in the customized
               processing scripts)

.. [#] Final Footnote.
