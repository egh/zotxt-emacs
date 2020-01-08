# zotxt-emacs

Note: zotero-plain has been split into two parts: zotxt-emacs (this
repository) for working with org, and `zot4rst` (zotero reStructuredText
tools), which been moved to [zot4rst](http://gitlab.com/egh/zot4rst).

## Installation

zotxt-emacs can be installed via [MELPA](http://melpa.milkbox.net/);
please see the `zotxt` package.

If installing without MELPA, please note that zotxt-emacs depends on https://github.com/tkf/emacs-request

## Requirements

zotxt-emacs requires GNU emacs &gt;= 24.3, or any version of emacs 25,
including unreleased versions. It should work with a builtin org-mode or
with the latest org-mode installed via the org ELPA repository.

## Emacs integration

Emacs integration has 3 parts: a core library (`zotxt.el`),
[org-mode](http://orgmode.org/) integration (`org-zotxt.el`), and an
“easykey” mode (`zotxt-easykey.el`). Emacs integration depends on the
[zotxt](http://gitlab.com/egh/zotxt) Zotero extension.

## For pandoc markdown files

zoxtxt-easykey, in combination with pandoc-zotxt, can help you edit your
pandoc markdown files. Load the zotxt-easykey minor mode using
`M-x zotxt-easykey-mode` and get started by inserting an easykey using
`C-c " k`. This will prompt you for a search string, which will do a
quicksearch in your Zotero library. You will then be prompted to make a
selection from the results of that search. An easykey for your selection
will then be inserted into your document.

### zotxt + org-mode

To insert a citation into a [org-mode](http://orgmode.org/) document,
first enable the `org-zotxt` minor mode:

    M-x org-zotxt-mode

Then select one or more items in [Zotero](http://www.zotero.org/) pane.
Finally, in [emacs](http://www.gnu.org/software/emacs/), use: `C-c " i`
to insert these items as citations. This inserts a zotero link with
descriptive link text.

To update the link text to reflect changed metadata from
[Zotero](http://www.zotero.org/), use `C-c " u` over the link.

## zotxt EasyKey integration

zotxt supports a feature known as “easy keys” for integration with
zotero and pandoc or zot4rst. These keys look like `@doe:2014title`
where Doe is the author’s last name, 2014 is the publication year, and
title is the first word of the title (excluding “stop words”, such as
“the” or “a”). The provided `zotxt-easykey-mode` supports completion of
these easykeys in a buffer using the command `completion-at-point`.
(This function is not bound in org-mode, but I replace the binding to
`pcomplete` using:

    ;; prefer completion-at-point to pcomplete
    (define-key org-mode-map (kbd "C-M-i") 'completion-at-point)

Now, the user can `@doe` and then `C-M-i`. If the only matching easy key
is `@doe:2014title` this will be completed. If there are multiple
matches, the user will be presented with a buffer containing possible
completions.
