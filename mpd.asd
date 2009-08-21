;;; -*- Mode: Lisp -*-

(defsystem #:mpd
  :name "mpd"
  :description "MPD client"
  :serial t
  :depends-on (#:usocket)
  :components ((:file "package")
               (:file "classes")
               (:file "mpd")
               (:file "commands")))
