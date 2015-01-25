Feature: Use org-zotxt-mode 
  In order to write academic documents
  As a user
  I want to access Zotero from org-mode

  Scenario: Turn on org-zotxt-mode 
    Given I am in buffer "foo"
    When I turn on org-mode
    When I turn on org-zotxt-mode
    Then org-zotxt-mode should be active
