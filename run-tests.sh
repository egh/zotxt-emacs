#!/bin/sh

set -ex

# Cleanup mock server on exit
trap "exit" INT TERM
trap "kill 0" EXIT

bundle exec ruby mock-server.rb -p 33119&
sleep 2

cd casks/"$CASK" || exit
cask clean-elc
cask
cask build
cask exec ert-runner
cask exec ecukes
