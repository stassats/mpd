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

(defmacro make-class (name slots)
  `(defclass ,name ()
     ,(loop for i in slots
	 collect `(,i :initform nil :initarg ,(to-keyword i)
		      :accessor ,(intern (format nil "~A-~A" name i))))))

(make-class track (file title artist
			album date track time pos id genre))

(defmethod print-object ((object track) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (artist title album) object
      (format stream "~A - ~A (~A)" artist title album))))

(defun connect (&key (host *defualt-host*) (port *default-port*))
  (let ((connection (socket-connect host port)))
    (values connection
	    (read-answer (socket-stream connection)))))

(defun send-command (connection command)
  (let ((stream (socket-stream connection)))
    (write-line command stream)
    (force-output stream)
    (read-answer stream)))

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
       (socket-close ,var))))

(defun split-values (current-playing)
  (mapcan (lambda (x)
	    (destructuring-bind (key value) (split ": " x)
	      (list (to-keyword key) value)))
	  current-playing))

(defun parse-track (data)
  (apply 'make-instance 'track (split-values data)))

(defun now-playing (connection)
  (parse-track (send-command connection "currentsong")))
