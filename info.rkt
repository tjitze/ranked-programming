#lang info
(define collection "ranked-programming")
(define deps '("sandbox-lib"
               "scribble-lib"
               "srfi-lite-lib"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/ranked-programming.scrbl" ())))
(define pkg-desc "Ranked Programming for Racket")
(define version "1.0")
(define pkg-authors '(tjitze))
