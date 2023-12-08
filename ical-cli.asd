(defpackage :ical-cli
  (:use :cl :asdf))
(in-package :ical-cli)

(defsystem "ical-cli"
  :name "ical-cli"
  :long-name "ical-cli"
  :description "iCal command line interface"
  :version "0.1.0"
  :author "Arda Köcer <arda.koecer@st.oth-regensburg.de>"
  :depends-on (:clingon :local-time :cl-date-time-parser)
  :components ((:module "main"
		:pathname #P"src/"
		:components ((:file "package")
           (:file "import")
	   (:file "parse")
           (:file "main"))))
  :build-operation "program-op"
  :build-pathname "bin/ical-cli"
  :entry-point "ical-cli:main")
