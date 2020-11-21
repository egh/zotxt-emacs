Feature: Use zotxt-citekey-mode
  In order to write academic markdown documents
  As a user
  I want to access Zotero from org-mode

  Background:
    Given I turn on zotxt-citekey-mode

  Scenario: Insert citekey
    Given I clear the buffer
    When I start an action chain
    # Cucumber cannot handle "C-c \" i" as a keybinding because of double quote
    And I press "M-x"
    And I type "zotxt-citekey-insert"
    And I press "RET"
    And I press "RET"
    And I type "doe"
    And I press "RET"
    And I type "Doe, John - First Book"
    And I press "RET"
    And I execute the action chain
    Then I should see "@doe:2005first"
