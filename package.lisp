;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)

(defpackage :mpd
  (:use :cl :usocket :cl-ppcre)
  (:export
   :connect
   :with-mpd-connection
   :now-playing))
