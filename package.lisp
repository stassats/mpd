;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)

(defpackage :mpd
  (:use :cl :usocket :cl-ppcre)
  (:export
   :connect
   :disconnect
   :with-mpd

   :now-playing
   :get-playlist
   :add-to-playlist

   :track
   :track-file
   :track-title
   :track-artist
   :track-album
   :track-date
   :track-track
   :track-time
   :track-pos
   :track-id
   :track-genre))
