#!/bin/sh
set -ex

cd casks/"$CASK" || exit
cask clean-elc
cask
cask build
cask exec ert-runner
cask exec ecukes

