#lang racket
(require ranked-programming)

; the robot localisation example from the paper

(define (neighbouring s)
  (let ((x (first s))
        (y (second s)))
    (either-of (list (list (- x 1) y)
                     (list (+ x 1) y)
                     (list x (- y 1))
                     (list x (+ y 1))))))

(define (surrounding s)
  (let ((x (first s))
        (y (second s)))
    (either-of (list (list (- x 1) (- y 1))
                     (list (- x 0) (- y 1))
                     (list (+ x 1) (- y 1))
                     (list (+ x 1) (- y 0))
                     (list (+ x 1) (+ y 1))
                     (list (- x 0) (+ y 1))
                     (list (- x 1) (+ y 1))
                     (list (- x 1) (- y 0))))))

(define (init) (list 0 0))

(define (next-state s) (neighbouring s))

(define (observable s) (nrm/exc s (surrounding s)))

; a generic implementation of a hidden markov model
(define (hmm obs)
  (if (empty? obs)
      ($ list (init))
      ($ cdr
         (observe (lambda (x) (equal? (car x) (car obs)))
                  (rlet* ((p (hmm (cdr obs)))
                          (s (next-state (car p)))
                          (o (observable s)))
                         (cons o (cons s p)))))))

; Inference example 1
(pr (hmm `((0 3) (2 3) (3 3) (3 2) (4 1) (2 1) (3 0) (1 0))))


