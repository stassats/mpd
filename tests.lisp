(defpackage #:mpd-tests
  (:use #:cl #:mpd #:lisp-unit))

(in-package #:mpd-tests)

(import '(mpd::split-value mpd::split-values mpd::filter-keys mpd::make-track))

(define-test play
  (assert-error 'type-error (play nil 'f)))

(define-test password
  (assert-error 'type-error (password nil 'f)))

(define-test output
  (assert-error 'type-error (disable-output nil "f"))
  (assert-error 'type-error (enable-output nil "f")))

(define-test playlist
  (assert-error 'type-error (list-playlist nil 10))
  (assert-error 'type-error (list-playlist-info nil 10))
  (assert-error 'type-error (save-playlist nil 10))
  (assert-error 'type-error (load-playlist nil 10))
  (assert-error 'type-error (playlist-info nil #\c))
  (assert-error 'type-error (rename-playlist nil 10 "f"))
  (assert-error 'type-error (rename-playlist nil "f" 10))
  (assert-error 'type-error (rename-playlist nil 10 10)))

(define-test add
  (assert-error 'simple-error (add nil 10))
  (assert-error 'simple-error (add-id nil 10)))

(define-test move
  (assert-error 'type-error (move nil 1 t))
  (assert-error 'type-error (move nil t 1))
  (assert-error 'type-error (move nil t t))
  (assert-error 'simple-error (move-id nil t t)))

(define-test filter-keys
  (assert-error 'type-error (filter-keys '("f")))
  (assert-error 'simple-error (filter-keys '("f:")))
  (assert-equal '("f" "g") (filter-keys '("f: f" "g: g"))))

(define-test split-value
  (assert-error 'type-error (split-value "f"))
  (assert-error 'simple-error (split-value "f:"))
  (assert-equal '(:f "f") (split-value "f: f"))

  (assert-equal '(:k "v" :k "v") (split-values '("k: v" "k: v"))))

(define-test make-track
  (assert-true (typep (make-track '("time: 10" "pos: 20" "id: 20") 'playlist) 'playlist))
  (assert-true (typep (make-track '("time: 10") 'track) 'track)))