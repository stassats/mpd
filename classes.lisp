;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package :mpd)

(define-condition mpd-error (error)
  ((text :initarg :text :reader text
	 :initform nil))
  (:report (lambda (condition stream)
	     (princ (text condition) stream))))

(define-condition protocol-mismatch (mpd-error)
  ())

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

(defconstant +error-ids-alist+
  '((2 . bad-argument)
    (3 . incorrect-password)
    (4 . not-permitted)
    (5 . unknown-command)
    (50 . not-exist)
    (51 . playlist-size-exceed)
    (54 . already-updating)
    (56 . exist)))

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
    :initform nil :initarg :time :accessor duration)))

(defclass playlist (track)
  ((pos
    :initform nil :initarg :pos :accessor position-in-playlist)
   (id
    :initform nil :initarg :id :accessor id)))

(defclass status ()
  ((artists         :accessor get-artists         :initarg :artists)
   (volume          :accessor get-volume          :initarg :volume)
   (repeat          :accessor get-repeat          :initarg :repeat)
   (random          :accessor get-random          :initarg :random)
   (playlist        :accessor get-playlist        :initarg :playlist)
   (playlist-length :accessor get-playlist-length :initarg :playlistlength)
   (xfade           :accessor get-xfade           :initarg :xfade)
   (state           :accessor get-state           :initarg :state)))

(defmethod print-object ((object track) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (artist title album) object
      (format stream "~A - ~A (~A)" artist title album))))

