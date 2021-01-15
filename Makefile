.PHONY: test

CASK ?= org-latest

test:
	$(MAKE) -C casks/$(CASK) test
