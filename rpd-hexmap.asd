;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :rpd-hexmap
  :serial t
  ;; add new files to this list:
  :components ((:file "package") (:file "rpd-hexmap"))
  :depends-on (#+nil :cl-ppcre))
