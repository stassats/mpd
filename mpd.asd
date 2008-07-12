;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:mpd-asd
  (:use :cl :asdf))

(in-package :mpd-asd)

(defsystem mpd
  :name "mpd"
  :description "MPD client"
  :serial t
  :depends-on (:usocket :alexandria)
  :components ((:file "package")
	       (:file "mpd")))
