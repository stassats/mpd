;;; -*- Mode: Lisp -*-

(in-package #:cl-user)

(defpackage #:mpd
  (:use #:cl)
  (:export
   #:*defualt-host*
   #:*default-port*
   #:connect
   #:disconnect
   #:password
   #:with-mpd
   #:disable-output
   #:enable-output
   #:outputs

   #:ping
   #:kill
   #:status

   #:now-playing
   #:pause
   #:play
   #:stop
   #:previous
   #:next
   #:crossfade

   #:add
   #:add-id
   #:move
   #:move-id
   #:swap
   #:swap-id
   #:clear
   #:delete-track
   #:delete-id
   #:save-playlist
   #:load-playlist
   #:rename-playlist
   #:playlist-info
   #:playlist-changes
   #:shuffle
   #:list-playlist
   #:list-playlist-info
   #:add-to-playlist
   #:clear-playlist
   #:delete-from-playlist
   #:move-in-playlist
   #:find-in-current-playlist
   #:search-in-current-playlist

   #:update

   #:list-all
   #:list-info
   #:list-all-info
   #:find-tracks
   #:search-tracks
   #:list-metadata
   #:count-tracks

   #:commands
   #:not-commands
   #:tag-types
   #:url-handlers

   #:playlist
   #:track
   #:file
   #:title
   #:artist
   #:album
   #:date
   #:duration
   #:genre
   #:composer

   #:position-in-playlist
   #:id

   #:mpd-error
   #:protocol-mismatch
   #:bad-argument
   #:incorrect-password
   #:not-permitted
   #:unknown-command
   #:not-exist
   #:playlist-size-exceed
   #:already-updating
   #:exist

   #:volume
   #:repeat
   #:randomized
   #:playlist-version
   #:playlist-length
   #:xfade
   #:state
   #:audio
   #:bitrate
   #:duration
   #:songid
   #:song

   #:artists
   #:albums
   #:songs
   #:uptime
   #:playtime
   #:db-playtime
   #:db-update))
