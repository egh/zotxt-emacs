# zotxt-emacs

## Introduction

zotxt-emacs works with [zotxt](https://github.com/egh/zotxt) to provide Emacs integration with Zotero, allowing you to manage citation keys for pandoc markdown documents as well as org mode links to items in your Zotero collection.

## Installation

zotxt-emacs depends on [Zotero](https://www.zotero.org/) and the [zotxt](https://github.com/egh/zotxt) Zotero addon, which must be installed first. [Better BibTeX](https://retorque.re/zotero-better-bibtex/) is required for pandoc and citekey integration.

zotxt-emacs can be installed via [MELPA](https://melpa.org/#/?q=zotxt) by installing the package named `zotxt`.

If installing without MELPA, please note that zotxt-emacs depends on [request.el](https://github.com/tkf/emacs-request) and [deferred.el](https://github.com/kiwanami/emacs-deferred)

## Requirements

zotxt-emacs requires GNU emacs >= 24.3. It should work with built-in org-mode or with the latest org-mode installed via the org ELPA repository.

zotxt-emacs requires Zotero and the zotxt addon >= 5.0.5.

# Org mode integration

To insert a link to a reference into a [org-mode](https://orgmode.org/) document, first enable the `org-zotxt` minor mode:

    M-x org-zotxt-mode

Then you can use: `C-c " i` (`org-zotxt-insert-reference-link`) to insert an item.

To update the current link text at point to reflect changed metadata from Zotero, use `C-c " u` (`org-zotxt-update-reference-link-at-point`).

To open an attachment of the link at point, use `C-c " a` (`org-zotxt-open-attachment`)

# Pandoc citekey integration

To insert citation keys into a markdown document (for use with pandoc), first enable  `zotxt-citekey` minor mode:

  M-x zotxt-citekey-mode
  
Then you can use: `C-c " k` (`zotxt-citekey-insert`) to insert a citation key. You can also use `complete-at-point` to complete the citekey. For example, if you type `@doe` `M-x completion-at-point`, you will be presented with a list of completions. In `markdown-mode` this is not bound to any key sequence, but you can of course bind it as you would like.

See [zotxt](https://github.com/egh/zotxt) for instructions on using [pandoc-zotxt.lua](https://github.com/odkr/pandoc-zotxt.lua) to automatically connect to a running Zotero instance to fetch citation data when using pandoc.

## org-noter

If you use [Org-noter](https://github.com/weirdNox/org-noter) you can use the `org-zotxt-noter` command to select an item in Zotero and take notes on its attachment. Use the command in a headline, and it will either open the Org-noter interface or prompt for a Zotero search to load an attachment that can be annotated using Org-noter.
