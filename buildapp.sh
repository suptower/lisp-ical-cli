sbcl --eval '(ql:register-local-projects)' \
     --eval '(ql:quickload :ical-cli)' \
     --eval '(asdf:make :ical-cli)' \
     --eval '(quit)'
