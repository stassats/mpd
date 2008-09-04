;;; -*- Mode: Lisp -*-

;;;; MPD Commands

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package :mpd)

(defcommand password (password)
  "Authentication."
  (check-args string password)
  (send "password" password))

(defcommand disconnect ()
  "Close connection."
  (socket-close connection))

(defcommand now-playing ()
  "Return instance of playlist with current song."
  (let ((track (send "currentsong")))
    (when track
      (make-track track 'playlist))))

(defcommand disable-output (id)
  (check-args integer id)
  (send "disableoutput" id))

(defcommand enable-output (id)
  (check-args integer id)
  (send "enableoutput" id))

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

;;; Control

(defcommand pause ()
  "Toggle pause / resume playing."
  (send "pause"))

(defcommand play (&optional song-number)
  (check-args (or integer null) song-number)
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

(defcommand list-playlist (name)
  "List files in the playlist `name'"
  (check-args string name)
  (filter-keys (send "listplaylist" name)))

(defcommand list-playlist-info (name)
  "List metadata of tracks in the playlist `name'"
  (check-args string name)
  (parse-list (send "listplaylistinfo" name) 'playlist))

(defcommand playlist ()
  "Return list of files in the current playlist."
  (filter-keys (send "playlist")))

(defcommand clear-playlist ()
  "Clear the current playlist."
  (send "clear"))

(defcommand save-playlist (filename)
  "Save the current playlist to the file in the playlist directory."
  (check-args string filename)
  (send "save" filename))

(defcommand load-playlist (filename)
  "Load playlist from file."
  (check-args string filename)
  (send "load" filename))

(defcommand rename-playlist (name new-name)
  "Rename playlist."
  (check-args string name new-name)
  (unless (equal name new-name)
    (send "rename" name new-name)))

(defcommand playlist-info (&optional id)
  "Return content of the current playlist."
  (check-args (or integer null) id)
  (if id
      (make-track (send "playlistinfo" id) 'playlist)
      (parse-list (send "playlistinfo") 'playlist)))

(defgeneric add (connection what)
  (:documentation "Add file or directory to the current playlist."))

(defmethod-command add ((what track))
  (add connection (file what)))

(defmethod-command add ((what string))
  (send "add" what))

(defgeneric add-id (connection what)
  (:documentation "Like add, but returns a id."))

(defmethod-command add-id ((what track))
  (add connection (file what)))

(defmethod-command add-id ((what string))
  (car (filter-keys (send "addid" what))))

(defcommand move (from to)
  "Move track from `from' to `to' in the playlist."
  (check-args integer from to)
  (unless (= from to)
    (send "move" from to)))

(defgeneric move-id (connection id to)
  (:documentation "Move track with `id' to `to' in the playlist."))

(defmethod-command move-id ((track playlist) (to integer))
  (move-id connection (id track) to))

(defmethod-command move-id ((id integer) (to integer))
  (send "moveid" id to))

(defcommand swap (first second)
  "Swap positions of two tracks."
  (check-args integer first second)
  (unless (= first second)
    (send "swap" first second)))

(defgeneric swap-id (connection first second)
  (:documentation "Swap positions of two tracks by id."))

(defmethod-command swap-id ((first playlist) (second playlist))
  (swap-id connection (id first) (id second)))

(defmethod-command swap-id ((first integer) (second integer))
  (send "swap" first second))

(defcommand delete-track (number)
  "Delete track from playlist."
  (check-args integer number)
  (send "delete" number))

(defgeneric delete-id (connection id)
  (:documentation "Delete track with `id' from playlist."))

(defmethod-command delete-id ((id playlist))
  (delete-id connection (id id)))

(defmethod-command delete-id ((id integer))
  (send "deleteid" id))

(defcommand shuffle ()
  "Shuffle the current playlist."
  (send "shuffle"))

;;; Database

(defcommand update (&optional path)
  "Scan directory for music files and add them to the database."
  (send "update" path))

(defcommand mpd-find (type what)
  "Find tracks in the database with a case sensitive, exact match."
  (assert (member type +tag-types+))
  (check-args string what)
  (parse-list (send "find" type what) 'track))

(defcommand mpd-list (metadata-1 &optional metadata-2 search-term)
  "List all metadata of `metadata-1'.
If `metadata-2' & `search-term' are supplied,
then list all `metadata-1' in which `metadata-2' has value `search-term'."
  (send "list" metadata-1 metadata-2 search-term))

(defcommand mpd-search (type what)
  "Find tracks in the database with a case sensitive, inexact match."
  (assert (member type +tag-types+))
  (check-args string what)
  (parse-list (send "search" type what) 'track))

(defcommand list-all-info (&optional path)
  "Lists all information about files in `path' recursively. Default path is /."
  (parse-list (send "listallinfo" path) 'track))

(defcommand list-all (&optional path)
  "Lists all files in `path' recursively. Default path is /."
  (check-args (or string null) path)
  (filter-keys (send "listall" path)))

(defcommand list-info (&optional path)
  "Show contents of directory."
  (check-args (or string null) path)
  (parse-list (send "lsinfo" path) 'track))

(defcommand mpd-count (scope query)
  "Number of songs and their total playtime matching `query'.
Return: (number playtime)."
  (filter-keys (send "count" scope query)))

(defcommand set-volume (value)
  "Set the volume to the value between 0-100."
  (check-type value (integer 0 100) "an integer in range 0-100")
  (send "setvol" value))

(defcommand tag-types ()
  "Get a list of available metadata types."
  (filter-keys (send "tagtypes")))

(defcommand url-handlers ()
  "Get a list of available URL handlers."
  (filter-keys (send "urlhandlers")))
