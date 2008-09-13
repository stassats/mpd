(defpackage #:mpd-tests
  (:use #:cl #:mpd #:lisp-unit))

(in-package #:mpd-tests)

(define-test play
  (assert-error 'type-error (play nil 'f)))

(define-test password
  (assert-error 'type-error (password nil 'f))
  (assert-error 'simple-error (password nil ""))
  (assert-error 'simple-error (password nil " ")))

(define-test output
  (assert-error 'type-error (disable-output nil "f"))
  (assert-error 'type-error (enable-output nil "f")))

(define-test playlist
  (assert-error 'type-error (list-playlist nil 10))
  (assert-error 'type-error (list-playlist-info nil 10))
  (assert-error 'type-error (save-playlist nil 10))
  (assert-error 'type-error (load-playlist nil 10))
  (assert-error 'simple-error (load-playlist nil " "))
  (assert-error 'type-error (playlist-info nil #\c))
  (assert-error 'type-error (rename-playlist nil 10 "f"))
  (assert-error 'type-error (rename-playlist nil "f" 10))
  (assert-error 'type-error (rename-playlist nil 10 10))
  (assert-error 'simple-error (rename-playlist nil " " " ")))

(define-test add
  (assert-error 'simple-error (add nil 10))
  (assert-error 'simple-error (add nil " "))
  (assert-error 'simple-error (add-id nil 10))
  (assert-error 'simple-error (add-id nil " ")))

(define-test move
  (assert-error 'type-error (move nil 1 t))
  (assert-error 'type-error (move nil t 1))
  (assert-error 'type-error (move nil t t))
  (assert-error 'simple-error (move-id nil t t)))

(define-test internals
  (assert-error 'type-error (mpd::split-value "f"))
  (assert-error 'simple-error (mpd::split-value "f:"))
  (assert-equal '(:f "f") (mpd::split-value "f: f"))

  (assert-equal '(:k "v" :k "v") (mpd::split-values '("k: v" "k: v")))

  (assert-error 'type-error (mpd::filter-keys '("f")))
  (assert-error 'simple-error (mpd::filter-keys '("f:")))
  (assert-equal '("f" "g") (mpd::filter-keys '("f: f" "g: g")))

  (assert-true (typep (mpd::make-track '("time: 10" "pos: 20" "id: 20") 'playlist) 'playlist))
  (assert-true (typep (mpd::make-track '("time: 10") 'track) 'track)))
