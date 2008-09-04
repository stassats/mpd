;;; -*- Mode: Lisp -*-

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

   :now-playing
   :pause
   :play
   :stop
   :previous
   :next
   :set-volume

   :add
   :add-id
   :move
   :move-id
   :swap
   :swap-id
   :clear-playlist
   :delete-track
   :delete-id
   :save-playlist
   :load-plalist
   :rename-playlist
   :playlist-info
   :shuffle
   :list-playlist
   :list-playlist-info

   :update
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
   :protocol-mismatch
   :bad-argument
   :incorrect-password
   :not-permitted
   :unknown-command
   :not-exist
   :playlist-size-exceed
   :already-updating
   :exist))
