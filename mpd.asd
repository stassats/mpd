;;; -*- Mode: Lisp -*-

(defpackage #:mpd-asd
  (:use #:cl #:asdf))

(in-package #:mpd-asd)

(defsystem #:mpd
  :name "mpd"
  :description "MPD client"
  :serial t
  :depends-on (#:usocket)
  :components ((:file "package")
               (:file "classes")
               (:file "mpd")
               (:file "commands")))
