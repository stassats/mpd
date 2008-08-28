;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package :mpd)

(defvar *defualt-host* "localhost")
(defvar *default-port* 6600)

(defun connect (&key (host *defualt-host*) (port *default-port*)
		password)
  "Connect to MPD."
  (let ((connection (socket-connect host port)))
    (prog1 (values connection
		   (read-answer (socket-stream connection)))
      (when password (password connection password)))))

(defun read-answer (stream)
  (loop
     for line = (read-line stream nil)
     until (string= line "OK" :end1 2)
     if (string= line "ACK" :end1 3) do
     (handle-error (subseq line 4))
     collect line))

(defun handle-error (text)
  (let* ((error-id (parse-integer text :start 1 :junk-allowed t))
	 (delimiter (position #\] text))
	 (condition (cdr (assoc error-id *error-ids-alist*))))
    (if (and delimiter condition)
	(error condition :text (subseq text (+ delimiter 2)))
	(error 'protocol-mismatch))))

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
	(error 'mpd-error :text (format nil "The stream ~A is not opened." stream)))))

(defun split-value (string)
  "Split a string 'key: value' into (list :key value)."
  (let ((column-position (position #\: string)))
    (unless column-position
      (error 'protocol-mismatch))
    (list (make-keyword
	   (string-upcase (subseq string 0 column-position)))
	  (subseq string (+ 2 column-position)))))

(defun split-values (strings)
  "Transform the list of strings 'key: value' into the plist."
  (mapcan #'split-value strings))

(defun filter-keys (strings)
  "Transform the list of strings 'key: value' into the list of values."
  (mapcar (lambda (entry)
	    (let ((column-position (position #\: entry)))
	      (unless column-position
		(error 'protocol-mismatch))
	      (subseq entry (+ 2 column-position))))
	  strings))

;;; C.f. performance:
;; (apply (lambda (&key foo bar) (make-instance
;; 	    'zot :quux 42 :foo foo :bar bar)) list)
(defun make-track (data type)
  "Make a new instance of the class playlist with initargs from
   the list of strings 'key: value'."
  (apply 'make-instance type (split-values data)))

(defun parse-list (list &optional class)
  "Make a list of new instances of the class `class' with initargs from
   a list of strings 'key: value'. Each track is separeted by the `file' key."
  (let (track)
    (labels ((create-track ()
	       (list
		(if class
		    (apply 'make-instance class track)
		    track))))
      (nconc
       (mapcan (lambda (x)
		 (let* ((pair (split-value x))
			(key (car pair)))
		   (cond ((and track (eq key :file))
			  (prog1 (create-track)
			    (setf track pair)))
			 ((or (eq key :directory) (eq key :playlist))
			  (list pair))
			 (t (setf track (nconc track pair))
			    nil))))
	       list)
       (when track (create-track))))))

;;; think about formatter
(defmacro send (&rest commands)
  `(send-command (format nil "~{~A~^ ~}"
			 (remove-if #'null (list ,@commands)))
		 connection))

(defmacro defcommand (name parameters &body body)
  (multiple-value-bind (forms decl doc) (parse-body body :documentation t)
    `(defun ,name (connection ,@parameters)
       ,@decl ,doc
       ,@forms)))

(defmacro defmethod-command (name parameters &body body)
  (multiple-value-bind (forms decl) (parse-body body)
    `(defmethod ,name (connection ,@parameters)
       ,@decl
       ,@forms)))

(defun process-string (string)
  (setf string
	(string-trim '(#\Space #\Tab #\Newline) string))
  (assert (> (length string) 0))
  (if (position #\Space string)
      (format nil "\"~a\"" string)
      string))
