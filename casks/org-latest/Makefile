.PHONY: test

test:
	cask clean-elc
	cask
	cask build
	cask exec ert-runner
	cask exec ecukes
