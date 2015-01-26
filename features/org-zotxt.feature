Feature: Use org-zotxt-mode
  In order to write academic documents
  As a user
  I want to access Zotero from org-mode

  Background:
    Given I turn on org-mode
    And I turn on org-zotxt-mode

  Scenario: Insert citation
    Given I start an action chain
    When I press "M-x"
    And I type "org-zotxt-insert-reference-link"
    And I press "RET"
    And I press "RET"
    And I type "doe"
    And I press "RET"
    And I type "Doe, John. “Article.” Journal of Generic Studies 6 (2006): 33–34."
    And I press "RET"
    And I execute the action chain
    And I wait for deferred tasks
    Then I should see "[[zotero://select/items/0_4T8MCITQ][Doe, John. “Article.” Journal of Generic Studies 6 (2006): 33–34.]]"

  Scenario: Update citation
    Given I insert "[[zotero://select/items/0_4T8MCITQ][foo]]"
    When I go to beginning of buffer
    And I type "C-c \" u"
    Then I should see "[[zotero://select/items/0_4T8MCITQ][Doe, John. “Article.” Journal of Generic Studies 6 (2006): 33–34.]]"
