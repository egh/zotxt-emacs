#!/bin/sh

set -ex

bundle exec ruby mock-server.rb -p 33119 &
MOCK_PID=$!
sleep 2

cd casks/"$CASK" || exit
cask clean-elc
cask
cask build
cask exec ert-runner
cask exec ecukes

kill "$MOCK_PID"
