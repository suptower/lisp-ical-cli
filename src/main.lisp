;;;; main.lisp
;;;;
;;;; This file contains the top-level command
(in-package :ical-cli)

;; specification
;; RFC 5545 https://datatracker.ietf.org/doc/html/rfc554

(defun top-level/command ()
  (clingon:make-command
   :name "ical-cli"
   :version "0.0.1"
   :description "A command line interface for iCal files specified by RFC 5545 (https://datatracker.ietf.org/doc/html/rfc5545).#\Newline The events are being stored in ~/.event_database."
   :authors '("Arda Köcer <arda.koecer@st.oth-regensburg.de>")
   :handler #'top-level/handler
   :sub-commands (top-level/sub-commands)))

(defun top-level/sub-commands ()
  (list
   (import/command)
   (show/command)
   (delete/command)
   (inspect/command)))

(defun top-level/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defun main ()
  (let ((app (top-level/command)))
    ;; to-do
    ;; every time app is run, clean up database (remove old events from database)
    (clingon:run app)))
