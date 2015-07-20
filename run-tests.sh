#/bin/sh -ex

# ensure no compile errors
rm -f ./*.elc
cask exec emacs -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
     -f batch-byte-compile zotxt.el
cask exec emacs -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
     -f batch-byte-compile org-zotxt.el
bundle install --quiet
cask
ruby mock-server.rb -p 33119  &
MOCK_PID=$!
sleep 2
cask exec ert-runner
cask exec ecukes
kill "$MOCK_PID"
while (kill -0 "$MOCK_PID" > /dev/null 2> /dev/null) ; do
    sleep 1
done
    
