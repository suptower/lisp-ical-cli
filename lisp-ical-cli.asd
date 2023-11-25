(defpackage :lisp-ical-cli
  (:use :cl :asdf))
(in-package :lisp-ical-cli)

(defsystem "lisp-ical-cli"
  :name "lisp-ical-cli"
  :long-name "lisp-ical-cli"
  :description "iCal command line interface"
  :version "0.1.0"
  :author "Arda Köcer <arda.koecer@st.oth-regensburg.de>"
  :depends-on (:clingon)
  :components ((:module "main"
		:pathname #P"src/"
		:components ((:file "package")
           (:file "import")
           (:file "main"))))
  :build-operation "program-op"
  :build-pathname "bin/lisp-ical-cli"
  :entry-point "lisp-ical-cli:main")
