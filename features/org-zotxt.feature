Feature: Use org-zotxt-mode
  In order to write academic documents
  As a user
  I want to access Zotero from org-mode

  Background:
    Given I turn on org-mode
    And I turn on org-zotxt-mode

  Scenario: Insert citation
    Given I clear the buffer
    When I start an action chain
    # Cucumber cannot handle "C-c \" i" as a keybinding because of double quote
    And I press "M-x"
    And I type "org-zotxt-insert-reference-link"
    And I press "RET"
    And I press "RET"
    And I type "doe"
    And I press "RET"
    And I type "Doe, John - Article"
    And I press "RET"
    And I execute the action chain
    Then I should see "[[zotero://select/items/0_4T8MCITQ][Doe, John. “Article.” Journal of Generic Studies 6 (2006): 33–34.]]"

  Scenario: Update citation
    Given I clear the buffer
    And I insert "[[zotero://select/items/0_ZBZQ4KMP][foo]]"
    When I go to beginning of buffer
    And I start an action chain
    And I press "M-x"
    And I type "org-zotxt-update-reference-link-at-point"
    And I press "RET"
    And I execute the action chain
    Then I should see "[[zotero://select/items/0_ZBZQ4KMP][Doe, John. First Book. Cambridge: Cambridge University Press, 2005.]]"

  Scenario: Update all citations
    Given I clear the buffer
    And I insert "[[zotero://select/items/0_ZBZQ4KMP][foo]]\n[[zotero://select/items/0_TWCW4IJ7][bar]]"
    When I go to beginning of buffer
    And I start an action chain
    And I press "M-x"
    And I type "org-zotxt-update-all-reference-links"
    And I press "RET"
    And I execute the action chain
    Then I should see "[[zotero://select/items/0_ZBZQ4KMP][Doe, John. First Book. Cambridge: Cambridge University Press, 2005.]]"
    And I should see "[[zotero://select/items/0_TWCW4IJ7][Doe, John, and Jenny Roe. “Why Water Is Wet.” In Third Book, edited by Sam Smith. Oxford: Oxford University Press, 2007.]]"
