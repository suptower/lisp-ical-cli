(require :uiop)
(ql:register-local-projects)
(ql:quickload :clingon)
;; (in-package :cl-user)
;; (defpackage :ical-cli
;;   (:use :cl)
;;   (:import-from :clingon)
;;   (:export :main))
;; (in-package :ical-cli)

(defpackage :ical-cli
  (:use :cl :asdf))
(in-package :ical-cli)

(defsystem "ical.cli"
  :name "ical.cli"
  :long-name "ical.cli"
  :description "iCal command line interface"
  :version "0.1.0"
  :author "Arda Köcer <arda.koecer@st.oth-regensburg.de>"
  :depends-on (:clingon)
  :components ((:module "main"
                :pathname #P"ical-cli.lisp"
                :components ((:file "main")))))
  :build-operation "program-op"
  :build-pathname "ical-cli"
  :entry-point "ical.cli:main")