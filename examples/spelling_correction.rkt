#lang racket
(require ranked-programming)

; the hidden markov model example from the paper

; A simple toy-example of a ranking-based hidden markov model specified through
; the parameters init, trans and emit.
(define (init)
  (either/or "rainy" "sunny"))
(define (trans s)
  (case s
    (("rainy") (nrm/exc "rainy" "sunny" 2))
    (("sunny") (nrm/exc "sunny" "rainy" 2))))
(define (emit s)
  (case s
    (("rainy") (nrm/exc "yes" "no" 1))
    (("sunny") (nrm/exc "no" "yes" 1))))

; a generic implementation of a hidden markov model
(define (hmm obs)
  (if (empty? obs)
      ($ list (init))
      ($ cdr
         (observe (lambda (x) (eq? (car x) (car obs)))
                  (rlet* ((p (hmm (cdr obs)))
                          (s (trans (car p)))
                          (o (emit s)))
                         (cons o (cons s p)))))))

; Inference example 1
(pr (hmm `("no" "no" "yes" "no" "no")))

; Inference example 2
(pr (hmm `("yes" "yes" "yes" "no" "no")))

