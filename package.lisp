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
   :play
   :stop
   :previous
   :next

   :now-playing
   :get-playlist
   :add
   :clear-playlist
   :delete-track
   :save-playlist
   :load-plalist
   :rename-playlist
   :update
   :playlist-info

   :track
   :file
   :title
   :artist
   :album

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
