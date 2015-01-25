#/bin/sh
ruby mock-server.rb -p 33119 &
MOCK_PID=$!
sleep 2
cask exec ert-runner
kill $MOCK_PID
