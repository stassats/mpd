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
  (let ((stream (socket-stream connection)))
    (if (open-stream-p stream)
	(progn
	  (write-line command stream)
	  (force-output stream)
	  (read-answer stream))
	(error (format nil "The stream ~A is not opened" stream)))))

(defmacro generate-commands (&rest commands)
  `(progn
     ,@(mapcar (lambda (x)
		 (let ((name
			(if (symbolp x)
			    x
			    (car x)))
		       (command
			(if (symbolp x)
			    (string-downcase (string x))
			    (cadr x))))
		   `(defun ,name (connection)
		      (send-command ,command connection))))
	       commands)))

(generate-commands
 (current-song "currentsong")
 pause stop next previous clear
 ping kill)

(defun split-values (current-playing)
  (mapcan (lambda (x)
	    (destructuring-bind (key value) (split ": " x)
	      (list (to-keyword key) value)))
	  current-playing))

(defun parse-track (data)
  (apply 'make-instance 'playlist (split-values data)))

(defun now-playing (connection)
  (parse-track (send-command "currentsong" connection)))

(defun get-playlist (connection)
  (mapcar
   (lambda (entry)
     (regex-replace "^\\d+:" entry ""))
   (send-command "playlist" connection)))
