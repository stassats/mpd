;;; -*- Mode: Lisp -*-

;;;; MPD Commands

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:mpd)

(defcommand password (password)
  "Authentication."
  (check-args string password)
  (send "password" password))

(defcommand disconnect ()
  "Close connection."
  (usocket:socket-close connection))

(defcommand now-playing ()
  "Return instance of playlist with current song."
  (let ((track (send "currentsong")))
    (when track
      (make-class track 'playlist))))

(defcommand disable-output (id)
  (check-args unsigned-byte id)
  (send "disableoutput" id))

(defcommand enable-output (id)
  (check-args unsigned-byte id)
  (send "enableoutput" id))

(defcommand ping ()
  "Send ping to MPD."
  (send "ping"))

(defcommand kill ()
  "Stop MPD in a safe way."
  (send "kill"))

(defcommand status ()
  "Return status of MPD."
  (make-class (send "status") 'status))

(defcommand stats ()
  "Return statisics."
  (make-class (send "stats") 'stats))

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
  (check-args (or unsigned-byte null) song-number)
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

(defcommand crossfade (seconds)
  (check-args unsigned-byte seconds)
  "Sets crossfading between songs."
  (send "crossfade" seconds))

;; Playlist

(defcommand list-playlist (name)
  "List files in the playlist `name'"
  (check-args string name)
  (filter-keys (send "listplaylist" name)))

(defcommand list-playlist-info (name)
  "List metadata of tracks in the playlist `name'"
  (check-args string name)
  (parse-list (send "listplaylistinfo" name) 'playlist))

(defcommand clear ()
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
  (check-args (or unsigned-byte null) id)
  (if id
      (make-class (send "playlistinfo" id) 'playlist)
      (parse-list (send "playlistinfo") 'playlist)))

(defcommand playlist-changes (version)
  "Return changed songs currently in the playlist since `version'."
  (check-args unsigned-byte version)
  (parse-list (send "plchanges" version) 'playlist))

(defcommand add-to-playlist (name path)
  "Add `path' to the playlist `name'."
  (check-args string name path)
  (send "playlistadd" name path))

(defcommand clear-playlist (name)
  "Clear playlist `name'."
  (check-args string name)
  (send "playlistclear"))

(defcommand delete-from-playlist (name song-id)
  "Delete `song-id' from playlist `name'."
  (check-args string name)
  (check-args unsigned-byte song-id)
  (send "playlistdelete" name song-id))

(defcommand move-in-playlist (name song-id position)
  "Move `song-id' in playlist `name' to `position'."
  (check-args string name)
  (check-args unsigned-byte song-id position)
  (send "playlistmove" name song-id position))

(defcommand find-in-current-playlist (scope query)
  "Search for songs in the current playlist with strict matching."
  (check-args string scope query)
  (send "playlistfind" scope query))

(defcommand search-in-current-playlist (scope query)
  "Search case-insensitively with partial matches for songs in the current playlist"
  (check-args string scope query)
  (send "playlistsearch" scope query))

(defgeneric add (connection what)
  (:documentation "Add file or directory to the current playlist."))

(defmethod-command add ((what track))
  (add connection (file what)))

(defmethod-command add ((what string))
  (check-args string what)
  (send "add" what))

(defgeneric add-id (connection what)
  (:documentation "Like add, but returns a id."))

(defmethod-command add-id ((what track))
  (add connection (file what)))

(defmethod-command add-id ((what string))
  (check-args string what)
  (car (filter-keys (send "addid" what))))

(defcommand move (from to)
  "Move track from `from' to `to' in the playlist."
  (check-args unsigned-byte from to)
  (unless (= from to)
    (send "move" from to)))

(defgeneric move-id (connection id to)
  (:documentation "Move track with `id' to `to' in the playlist."))

(defmethod-command move-id ((track playlist) (to integer))
  (move-id connection (id track) to))

(defmethod-command move-id ((id integer) (to integer))
  (check-args unsigned-byte id to)
  (send "moveid" id to))

(defcommand swap (first second)
  "Swap positions of two tracks."
  (check-args unsigned-byte first second)
  (unless (= first second)
    (send "swap" first second)))

(defgeneric swap-id (connection first second)
  (:documentation "Swap positions of two tracks by id."))

(defmethod-command swap-id ((first playlist) (second playlist))
  (swap-id connection (id first) (id second)))

(defmethod-command swap-id ((first integer) (second integer))
  (check-args unsigned-byte first second)
  (send "swap" first second))

(defcommand delete-track (number)
  "Delete track from playlist."
  (check-args unsigned-byte number)
  (send "delete" number))

(defgeneric delete-id (connection id)
  (:documentation "Delete track with `id' from playlist."))

(defmethod-command delete-id ((id playlist))
  (delete-id connection (id id)))

(defmethod-command delete-id ((id integer))
  (check-args unsigned-byte id)
  (send "deleteid" id))

(defcommand shuffle ()
  "Shuffle the current playlist."
  (send "shuffle"))

;;; Database

(defcommand update (&optional path)
  "Scan directory for music files and add them to the database."
  (check-args string path)
  (send "update" path))

(defcommand find-tracks (type what)
  "Find tracks in the database with a case sensitive, exact match."
  (assert (member type *tag-types*))
  (check-args string what)
  (parse-list (send "find" type what) 'track))

(defcommand list-metadata (metadata-1 &optional metadata-2 search-term)
  "List all metadata of `metadata-1'.
If `metadata-2' & `search-term' are supplied,
then list all `metadata-1' in which `metadata-2' has value `search-term'."
  (check-args string search-term)
  (send "list" metadata-1 metadata-2 search-term))

(defcommand search-tracks (type what)
  "Find tracks in the database with a case sensitive, inexact match."
  (assert (member type *tag-types*))
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

(defcommand count-tracks (scope query)
  "Number of songs and their total playtime matching `query'.
Return: (number playtime)."
  (check-args string query)
  (filter-keys (send "count" scope query)))

(defcommand tag-types ()
  "Get a list of available metadata types."
  (filter-keys (send "tagtypes")))

(defcommand url-handlers ()
  "Get a list of available URL handlers."
  (filter-keys (send "urlhandlers")))

(defun (setf volume) (value connection)
  "Set the volume to the value between 0-100."
  (check-type value (integer 0 100) "an integer in range 0-100")
  (send "setvol" value))

(defun (setf randomized) (value connection)
  "NIL---turn off random mode, non-nil---turn on random mode."
  (send "random" (if value 1 0)))

(defun (setf repeat) (value connection)
  "NIL---turn off repeat mode, non-nil---turn on repeat mode."
  (send "repeat" (if value 1 0)))

(defcommand seek (song time)
  "Skip to a specified point in a song on the playlist."
  (send "seek" song time))

(defgeneric seek-id (connection song time)
  (:documentation "Skip to a specified point in a song on the playlist."))

(defmethod-command seek-id ((song playlist) (time integer))
  (seek-id connection (id song) time))

(defmethod-command seek-id ((song integer) (time integer))
  (check-args unsigned-byte song time)
  (send "seekid" song time))
