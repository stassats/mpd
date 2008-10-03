;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:mpd)

(define-condition mpd-error (error)
  ((text :initarg :text :reader text
         :initform nil))
  (:report (lambda (condition stream)
             (princ (text condition) stream))))

(define-condition unknown-command (mpd-error)
  ())

(define-condition incorrect-password (mpd-error)
  ())

(define-condition bad-argument (mpd-error)
  ())

(define-condition not-permitted (mpd-error)
  ())

(define-condition not-exist (mpd-error)
  ())

(define-condition playlist-size-exceed (mpd-error)
  ())

(define-condition already-updating (mpd-error)
  ())

(define-condition exist (mpd-error)
  ())

(define-constant +error-ids-alist+
  '((2 . bad-argument)
    (3 . incorrect-password)
    (4 . not-permitted)
    (5 . unknown-command)
    (50 . not-exist)
    (51 . playlist-size-exceed)
    (54 . already-updating)
    (56 . exist))
  :test #'equal)

(define-constant +tag-types+
  '(:artist :album :title :track :name :genre :date
    :composer :performer :comment :disc :filename :any)
  :test #'equal
  :documentation "Types of tags for using in `search' and `find'")

(defclass track ()
  ((file
    :initform nil :initarg :file :accessor file)
   (title
    :initform nil :initarg :title :accessor title)
   (artist
    :initform nil :initarg :artist :accessor artist)
   (album
    :initform nil :initarg :album :accessor album)
   (genre
    :initform nil :initarg :genre :accessor genre)
   (date
    :initform nil :initarg :date :accessor date)
   (performer
    :initform nil :initarg :performer :accessor performer)
   (composer
    :initform nil :initarg :composer :accessor composer)
   (disc
    :initform nil :initarg :disc :accessor disc)
   (track
    :initform nil :initarg :track :accessor track-number)
   (time
    :initform nil :initarg :time :accessor duration
    :type integer)))

(defclass playlist (track)
  ((pos
    :initform nil :initarg :pos :accessor position-in-playlist
    :type integer)
   (id
    :initform nil :initarg :id :accessor id
    :type integer)))

(defclass status ()
  ((volume 
    :reader volume :initarg :volume :initform nil)
   (repeat 
    :reader repeat :initarg :repeat :initform nil)
   (random
    :reader randomized :initarg :random :initform nil)
   (playlist
    :reader playlist-id :initarg :playlist :initform nil)
   (playlist-length
    :reader playlist-length :initarg :playlistlength :initform nil)
   (xfade
    :reader xfade :initarg :xfade :initform nil)
   (state
    :reader state :initarg :state :initform nil)
   (audio
    :reader audio :initarg :audio :initform nil)
   (bitrate
    :reader bitrate :initarg :bitrate :initform nil)
   (time
    :reader duration :initarg :time :initform nil)
   (songid
    :reader songid :initarg :songid :initform nil)
   (song :reader song :initarg :song :initform nil)))

(defmacro generate-status-commands (names)
  `(progn
     ,@(mapcar (lambda (name)
                 `(defmethod ,name ((stream stream-usocket))
                    (,name (status stream))))
               names)))

(generate-status-commands
 (volume repeat randomized playlist-id playlist-length
  xfade state audio bitrate duration songid song))

(defparameter *integer-keys*
  '(:id :pos :volume :repeat :random :playlist
    :xfade :song :songid :bitrate :playlistlength
    :artists :albums :songs :uptime :playtime
    :db_playtime :db_update)
  "List of keys which values must be integers.")

(defparameter *value-processing-functions*
  '(:time parse-time :state to-keyword))

(defmethod print-object ((object track) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (artist title album) object
      (format stream "~A - ~A (~A)" artist title album))))
