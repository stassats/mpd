(in-package :mpd)

(defparameter *defualt-host* "localhost")
(defparameter *default-port* 6600)

(defun to-keyword (symbol)
  (intern (string-upcase (string symbol)) :keyword))

(defmacro make-class (name slots)
  `(defclass ,name ()
     ,(loop for i in slots
	 collect `(,i :initform nil :initarg ,(to-keyword i)))))

(make-class track (file title artist album date track time pos id))

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

(defmacro with-mpd-connection ((var &rest options) &body body)
  `(let ((,var (connect ,@options)))
     (unwind-protect 
	  (progn ,@body)
       (socket-close ,var))))

(defun split-values (current-playing)
  (mapcan (lambda (x) 
	    (destructuring-bind (key value) (split ": " x)
	      (list (symbol-to-keyword key) value)))
	  current-playing))

(defun parse-track (data)
  (apply 'make-instance 'track (split-values data)))

(defun now-playing (connection)
  (with-slots (title artist album date)
      (parse-track (send-command connection "currentsong"))
    (format nil "~A - ~A (~A~@[; ~A~])" artist title album date)))
