;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:mpd)

(define-condition mpd-error (error)
  ((text :initarg :text :reader text
         :initform nil))
  (:report (lambda (condition stream)
             (princ (text condition) stream))))

(macrolet ((define-conditions (names)
             `(progn ,@(mapcar
                        (lambda (name)
                          `(define-condition ,name (mpd-error) ()))
                        names))))
  (define-conditions (bad-argument incorrect-password
                      not-permitted unknown-command not-exist
                      playlist-size-exceed already-updating exist)))

(defparameter *error-ids-alist*
  '((2 . bad-argument)
    (3 . incorrect-password)
    (4 . not-permitted)
    (5 . unknown-command)
    (50 . not-exist)
    (51 . playlist-size-exceed)
    (54 . already-updating)
    (56 . exist)))

(defparameter *tag-types*
  '(:artist :album :title :track :name :genre :date
    :composer :performer :comment :disc :filename :any)
  "Types of tags for using in `search' and `find'")

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
    :initform nil :initarg :time :accessor duration)
   (last-modified
    :initform nil :initarg :last-modified :accessor last-modified)))

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
    :reader playlist-version :initarg :playlist :initform nil)
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

(defclass stats ()
  ((artists
    :reader artists :initarg :artists :initform nil)
   (albums
    :reader albums :initarg :albums :initform nil)
   (songs
    :reader songs :initarg :songs :initform nil)
   (uptime
    :reader uptime :initarg :uptime :initform nil)
   (playtime
    :reader playtime :initarg :playtime :initform nil)
   (db-playtime
    :reader db-playtime :initarg :db_playtime :initform nil)
   (db-update
    :reader db-update :initarg :db_update :initform nil)))

(macrolet ((generate-commands (class names)
             `(progn
                ,@(mapcar (lambda (name)
                            `(defmethod ,name ((stream usocket:stream-usocket))
                               (,name (,class stream))))
                          names))))
  (generate-commands status
                     (volume repeat randomized playlist-version playlist-length
                      xfade state audio bitrate duration songid song))
  (generate-commands stats
                     (artists albums songs uptime playtime db-playtime db-update)))

(defparameter *integer-keys*
  '(:id :pos :volume :playlist :playlistlength
    :xfade :song :songid :bitrate :playtime
    :artists :albums :songs :uptime :db_playtime :db_update
    :outputid)
  "List of keys which values must be integers.")

(defparameter *value-processing-functions*
  '(:time parse-time :state to-keyword
    :random string-not-zerop :repeat string-not-zerop
    :outputenabled string-not-zerop))

(defmethod print-object ((object track) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (artist title album) object
      (format stream "~A - ~A (~A)" artist title album))))
