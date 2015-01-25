#/bin/sh -e
bundle install
cask
ruby mock-server.rb -p 33119 &
MOCK_PID=$!
sleep 2
cask exec ert-runner
cask exec ecukes
kill $MOCK_PID
while (kill -0 $MOCK_PID > /dev/null 2> /dev/null) ; do
    sleep 1
done
    
