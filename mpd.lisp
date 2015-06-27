;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:mpd)

(defvar *defualt-host* "localhost")
(defvar *default-port* 6600)

(defun connect (&key (host *defualt-host*) (port *default-port*) password)
  "Connect to MPD."
  (let ((connection (usocket:socket-connect host port)))
    (prog1 (values connection
                   (read-answer (usocket:socket-stream connection)))
      (when password
        (password connection password)))))

(defun read-answer (stream)
  (loop for line = (read-line stream)
        until (string= line "OK" :end1 2)
        collect line
        when (string= line "ACK" :end1 3)
        do (throw-error line)))

(defun throw-error (text)
  ;; Error format: `ACK [<error id>@<position>] {<comand name>} <description>'
  (let* ((error-id (parse-integer text :start 5 :junk-allowed t))
         (delimiter (position #\] text))
         (condition (cdr (assoc error-id *error-ids-alist*))))
    (error condition :text (subseq text (+ delimiter 2)))))

(defmacro with-mpd ((var &rest options) &body body)
  `(let ((,var (connect ,@options)))
     (unwind-protect
          (progn ,@body)
       (disconnect ,var))))

(defun send-command (connection command)
  "Send command to MPD."
  (let ((stream (usocket:socket-stream connection)))
    (unless (open-stream-p stream)
      (error 'mpd-error :text (format nil "The stream ~A is not opened." stream)))
    (write-line command stream)
    (finish-output stream)
    (read-answer stream)))

;;; Parsing

(defun to-keyword (name)
  (intern (string-upcase name) :keyword))

(defun split-value (string)
  "Split a string `key: value' into (list :key value)."
  (let ((column (position #\: string)))
    (process-value (to-keyword (subseq string 0 column))
                   (subseq string (+ 2 column)))))

(defun split-values (strings)
  "Transform a list of strings 'key: value' into the plist."
  (mapcan #'split-value strings))

(defun process-value (key value)
  (list key
        (funcall (value-processing-function key) value)))

(defun value-processing-function (key)
  (if (member key *integer-keys*)
      #'parse-integer
      (getf *value-processing-functions* key #'identity)))

(defun parse-time (time)
  "\"10:20\" -> (10 20); \"10\" -> 10"
  (multiple-value-bind (first stop)
      (parse-integer time :junk-allowed t)
    (if (= stop (length time))
        first
        (list first
              (parse-integer time :start (1+ stop))))))

(defun string-not-zerop (string)
  (not (string= string "0")))

(defun filter-keys (strings)
  "Transform a list of strings 'key: value' into a list of values."
  (mapcar (lambda (entry)
            (subseq entry (+ 2 (position #\: entry))))
          strings))

(defun make-class (data type)
  "Make a new instance of the class playlist with initargs from
   the list of strings `key: value'."
  (apply 'make-instance type (split-values data)))

(defun parse-list (list &optional class)
  "Make a list of new instances of the class `class' with initargs from
   a list of strings `key: value'. Each track is separeted by the `file' key."
  (let (track)
    (flet ((create-track ()
             (when track
               (list (apply 'make-instance class track)))))
      (nconc
       (mapcan (lambda (x)
                 (let ((pair (split-value x)))
                   (case (car pair)
                     (:file (prog1 (create-track)
                              (setf track pair)))
                     ((:directory :playlist)
                      (list pair))
                     (t (nconc track pair)
                        nil))))
               list)
       (create-track)))))

;;;

(defun process-string (string)
  "Check for emtpy strings, and escape strings when needed."
  (when string
    (let ((string
           (string-trim '(#\Space #\Tab #\Newline) string)))
      (when (zerop (length string))
        (error 'mpd-error :text "Zero length argument."))
      (if (position #\Space string)
          (prin1-to-string string)
          string))))

;;; Macros

(defmacro send (&rest commands)
  "Macro for using inside `defcommand'."
  `(send-command connection
                 (format nil "~{~A~^ ~}"
                         (remove nil (list ,@commands)))))

(defmacro defcommand (name parameters &body body)
  `(defun ,name (connection ,@parameters)
     ,@body))

(defmacro defmethod-command (name parameters &body body)
  `(defmethod ,name (connection ,@parameters)
     ,@body))

(defmacro check-args (type &rest args)
  "Check string and integer arguments."
  (if (or (eq type 'string)
          (and (listp type)
               (member 'string type)))
      `(progn ,@(mapcan
                 (lambda (arg)
                   `((check-type ,arg ,type "a string")
                     (setf ,arg (process-string ,arg))))
                 args))
      `(progn ,@(mapcar
                 (lambda (arg)
                   `(check-type ,arg ,type))
                 args))))
