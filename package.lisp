;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)

(defpackage :mpd
  (:use :cl :usocket :alexandria)
  (:export
   :*defualt-host*
   :*default-port*
   :connect
   :disconnect
   :password
   :with-mpd
   :disable-output
   :enableoutput
   :outputs

   :ping
   :kill
   :status

   :pause
   :play
   :stop
   :previous
   :next

   :now-playing
   :add
   :add-id
   :move
   :move-id
   :clear-playlist
   :delete-track
   :delete-id
   :save-playlist
   :load-plalist
   :rename-playlist
   :update
   :playlist-info
   :set-volume
   :tag-types
   :url-handlers
   :list-all
   :list-info
   :list-all-info
   :commands
   :not-commands
   :mpd-find
   :mpd-list
   :mpd-search

   :track
   :file
   :title
   :artist
   :album
   :date
   :duration
   :genre
   :composer
   
   :playlist
   :position-in-playlist
   :id

   :mpd-error
   :bad-argument
   :incorrect-password
   :not-permitted
   :unknown-command
   :not-exist
   :playlist-size-exceed
   :already-updating
   :exist))
