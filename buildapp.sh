sbcl --eval '(ql:register-local-projects)' \
     --eval '(ql:quickload :lisp-ical-cli)' \
     --eval '(asdf:make :lisp-ical-cli)' \
     --eval '(quit)'
