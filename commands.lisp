;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; MPD Commands

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package :mpd)

(defcommand password (password)
  "Authentication."
  (send "password" password))

(defcommand disconnect ()
  "Close connection."
  (socket-close connection))

(defcommand now-playing ()
  "Return instance of playlist with current song."
  (parse-track (send "currentsong")))

;;; Control

(defcommand pause ()
  "Toggle pause / resume playing."
  (send "pause"))

(defcommand play (&optional song-number)
  "Begin playing the playlist starting from song-number, default is 0."
  (send "play" song-number))

(defcommand stop ()
  "Stop playing."
  (send "stop"))

(defcommand next ()
  "Play next track in the playlist."
  (send "next"))

(defcommand previous ()
  "Play previous track in the playlist."
  (send "previous"))

;; Playlist

(defcommand get-playlist ()
  "Return list of files in the current playlist."
  (filter-keys (send "playlist")))

(defcommand clear-playlist ()
  "Clear the current playlist."
  (send "clear"))

(defgeneric add (connection what)
  (:documentation "Add file or directory to the current playlist."))

(defmethod add (connection (what track))
  (add (track-file what) connection))

(defmethod add (connection (what string))
  (send-command (format nil "add ~a" what) connection))

(defcommand save-playlist (filename)
  "Save the current playlist to the file in the playlist directory."
  (send "save" filename))

(defcommand load-playlist (filename)
  "Load playlist from file."
  (send "load" filename))

(defcommand rename-playlist (name new-name)
  "Rename playlist."
  (send "rename" name new-name))

(defcommand playlist-info (&optional id)
  "Return content of the current playlist."
  (if id
      (parse-track (send "playlistinfo" id))
      (parse-list (send "playlistinfo") 'playlist)))

(defcommand delete-track (number)
  "Delete track from playlist."
  (send "delete" number))

(defcommand ping ()
  "Send ping to MPD."
  (send "ping"))

(defcommand kill ()
  "Stop MPD in a safe way."
  (send "kill"))

(defcommand status ()
  "Return status of MPD."
  (split-values (send "status")))

(defcommand stats ()
  "Return statisics."
  (split-values (send "stats")))

(defcommand outputs ()
  "Return information about all outputs."
  (split-values (send "outputs")))

(defcommand commands ()
  "Return list of available commands."
  (filter-keys (send "commands")))

(defcommand not-commands ()
  "Return list of commands to which the current user does not have access."
  (filter-keys
   (send "notcommands")))

;;; Database

(defcommand update (&optional path)
  "Scan directory for music files and add them to the database."
  (send "update" path))

(defcommand mpd-find (type what)
  "Find tracks in the database with a case sensitive, exact match."
  (send "find" type what))

(defcommand list-all-info (&optional path)
  (parse-list (send "listallinfo" path) 'track))

(defcommand list-all (&optional path)
  (parse-list (send "listall" path)))

(defcommand list-info (&optional path)
  "Show contents of directory."
  (parse-list (send "lsinfo" path) 'track))

(defcommand set-volume (value)
  "Set the volume to the value between 0-100."
  (declare ((integer 0 100) value))
  (send "setvol" value))

(defcommand tag-types ()
  "Get a list of available metadata types."
  (filter-keys (send "tagtypes")))

(defcommand url-handlers ()
  "Get a list of available URL handlers."
  (filter-keys (send "urlhandlers")))
