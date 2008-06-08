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

(defun split-value (value)
  (let ((match
	 (nth-value 1 (scan-to-strings "^(.+?): (.+)" value))))
    (values (aref match 0)
	    (aref match 1))))

(defun split-values (values)
  (mapcan (lambda (x)
	    (multiple-value-bind (key value) (split-value x)
	      (list (to-keyword key) value)))
	  values))

(defun parse-track (data)
  (apply 'make-instance 'playlist (split-values data)))

(defun parse-list (list)
  (let (track)
    (mapcan (lambda (x)
	      (multiple-value-bind (key value) (split-value x)
		(setf key (to-keyword key))
		(if (and track (eq key :file))
		    (prog1
			(list (apply 'make-instance 'playlist track))
		      (setf track
			    (list key value)))
		    (progn
		      (push value track)
		      (push key track)
		      nil))))
	    list)))

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
       (macrolet ((send (command)
		    `(send-command ,command
				  connection)))
	        ,@forms))))

(defcommand disconnect ()
  "Close connection."
  (socket-close connection))

(defcommand now-playing ()
  "Return instance of playlist with current song."
  (parse-track (send "currentsong")))

(defcommand pause ()
  "Toggle pause / resume playing."
  (send "pause"))

(defcommand stop ()
  "Stop playing."
  (send "stop"))

(defcommand next ()
  "Play next track in the playlist."
  (send "next"))

(defcommand previous ()
  "Play previous track in the playlist."
  (send "previous"))

(defcommand clear-playlist ()
  "Clear the current playlist."
  (send "clear"))

(defcommand ping ()
  "Send ping to MPD."
  (send "ping"))

(defcommand kill ()
  "Stop MPD in a safe way."
  (send "kill"))

(defmethod add ((what track) connection)
  (add (track-file what) connection))

(defmethod add ((what string) connection)
  "Add file or directory to the current playlist."
  (send-command (format nil "add ~a" what) connection))

(defcommand save-playlist (filename)
  "Save the current playlist to the file in the playlist directory."
  (send (format nil "save ~A" filename)))

(defcommand load-playlist (filename)
  "Load playlist from file."
  (send (format nil "load ~A" filename)))

(defcommand rename-playlist (name new-name)
  "Rename playlist."
  (send (format nil "rename ~A ~A" name new-name)))

(defcommand delete-track (number)
  "Delete track from playlist."
  (send (format nil "delete ~A" number)))

(defcommand update (path)
  "Scan directory for music files and add them to the database."
  (send (format nil "update ~A" path)))

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
  (mapcar
   (lambda (entry)
     (nth-value 1 (split-value entry)))
   (send "commands")))

(defcommand not-commands ()
  "Return list of commands to which the current user does not have access."
  (mapcar
   (lambda (entry)
     (nth-value 1 (split-value entry)))
   (send "notcommands")))

(defcommand mpd-find (type what)
  "Find tracks in the database with a case sensitive, exact match."
  (send (format nil "find ~A \"~A\"" type what)))

(defcommand playlist-info ()
  "Return content of the current playlist."
  (parse-list
   (send "playlistinfo")))
