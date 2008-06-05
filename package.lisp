;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)

(defpackage :mpd
  (:use :cl :usocket :cl-ppcre
	:alexandria)
  (:export
   :connect
   :disconnect
   :with-mpd

   :ping
   :kill
   :status

   :pause
   :stop
   :previous
   :next
   
   :now-playing
   :get-playlist
   :add
   :clear
   :delete-track
   :save-playlist
   :load-plalist
   :rename-playlist
   :update

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
