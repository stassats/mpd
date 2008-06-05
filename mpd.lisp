;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; This software is in the public domain and is
;; provided with absolutely no warranty.

(in-package :mpd)

(defparameter *defualt-host* "localhost")
(defparameter *default-port* 6600)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-keyword (x)
    "Coerce a string, a symbol, or a character to a keyword"
    (intern (string-upcase (string x)) :keyword)))

(defmacro make-class (name superclasses slots)
  `(defclass ,name ,superclasses
     ,(loop for i in slots
	 collect `(,i :initform nil :initarg ,(to-keyword i)
		      :accessor ,(intern (format nil "~A-~A" name i))))))

(make-class track () (file title artist album
			   genre date track time))

(make-class playlist (track)
	    (pos id))

(defmethod print-object ((object track) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (artist title album) object
      (format stream "~A - ~A (~A)" artist title album))))

(defun connect (&key (host *defualt-host*) (port *default-port*))
  "Connect to MPD."
  (let ((connection (socket-connect host port)))
    (values connection
	    (read-answer (socket-stream connection)))))

(defun disconnect (connection)
  (socket-close connection))

(defun read-answer (stream)
  (loop
     for line = (read-line stream nil)
     until (string= line "OK" :end1 2)
     if (string= line "ACK" :end1 3) do
     (error (subseq line 4))
     collect line))

(defmacro with-mpd ((var &rest options) &body body)
  `(let ((,var (connect ,@options)))
     (unwind-protect
	  (progn ,@body)
       (disconnect ,var))))

(defun send-command (command connection)
  "Send command to MPD."
  (let ((stream (socket-stream connection)))
    (if (open-stream-p stream)
	(progn
	  (write-line command stream)
	  (force-output stream)
	  (read-answer stream))
	(error (format nil "The stream ~A is not opened" stream)))))

(defun split-values (values)
  (mapcan (lambda (x)
	    (destructuring-bind (key value) (split ": " x)
	      (list (to-keyword key) value)))
	  values))

(defun parse-track (data)
  (apply 'make-instance 'playlist (split-values data)))

(defun now-playing (connection)
  "Return instance of playlist with current song."
  (parse-track (current-track connection)))

(defun get-playlist (connection)
  "Return list of files in the current playlist."
  (mapcar
   (lambda (entry)
     (regex-replace "^\\d+:" entry ""))
   (send-command "playlist" connection)))

(defmacro defcommand (name parameters &body body)
  (multiple-value-bind (forms decl doc) (parse-body body :documentation t)
    `(defun ,name (,@parameters connection)
       ,@decl
       ,doc
       (send-command ,@forms
		     connection))))

(defcommand current-track ()
  "Return the metadata of the current track."
  "currentsong")

(defcommand pause ()
  "Toggle pause / resume playing."
  "pause")

(defcommand stop ()
  "Stop playing."
  "stop")

(defcommand next ()
  "Play next track in the playlist."
  "next")

(defcommand previous ()
  "Play previous track in the playlist."
  "previous")

(defcommand clear ()
  "Clear the current playlist."
  "clear")

(defcommand ping ()
  "Send ping to MPD."
  "ping")

(defcommand kill ()
  "Stop MPD in a safe way.")

(defcommand add (what)
  "Add file or directory to the current playlist."
  (format nil "add ~A" what))

(defcommand save-playlist (filename)
  "Save the current playlist to the file in the playlist directory."
  (format nil "save ~A" filename))

(defcommand load-playlist "Load playlist from file."
  "load"
  filename)

(defcommand rename-playlist (name new-name)
  "Rename playlist."
  (format nil "rename ~A ~A" name new-name))

(defcommand delete-track (number connection)
  "Delete track from playlist."
  (format nil "delete ~A" number))

(defcommand update (path)
  "Scan directory for music files and add them to the database."
  (format nil "update ~A" path))

(defun status (connection)
  "Return status of MPD."
  (split-values (send-command "status" connection)))

(defun stats (connection)
  "Return statisics."
  (split-values (send-command "stats" connection)))

(defun outputs (connection)
  "Return information about all outputs."
  (split-values (send-command "outputs" connection)))

(defun commands (connection)
  "Return list of available commands."
  (mapcar
   (lambda (entry)
     (regex-replace "^command: " entry ""))
   (send-command "commands" connection)))

(defun not-commands (connection)
  "Return list of commands to which the current user does not have access."
  (mapcar
   (lambda (entry)
     (regex-replace "^command: " entry ""))
   (send-command "notcommands" connection)))

(defcommand mpd-find (type what)
  "Find tracks in the database with a case sensitive, exact match."
  (format nil "find ~A ~A" type what))
