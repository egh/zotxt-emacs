#/bin/sh -e

bundle install --quiet
ruby mock-server.rb -p 33119 >/dev/null 2>/dev/null &
MOCK_PID=$!
sleep 2

for CASK_DIR in casks/* ; do
    printf "Testing %s\n" "$(basename "${CASK_DIR}")"
    cd "$CASK_DIR" || exit
    cask clean-elc
    cask
    # ensure no compile errors
    cask exec emacs -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
         -f batch-byte-compile zotxt.el
    cask exec emacs -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
         -f batch-byte-compile org-zotxt.el
    cask exec ert-runner --quiet
    cask exec ecukes --quiet
    cd ../.. || exit
done

kill "$MOCK_PID"
while (kill -0 "$MOCK_PID" > /dev/null 2> /dev/null) ; do
    sleep 1
done
