#lang racket
(require ranked-programming)

; the boolean circuit example from the paper
(define (circuit i1 i2 i3 o)
 ($ cdr
  (observe
   (lambda (x) (eq? (car x) o))
   (rlet* ((N (nrm/exc #t #f))
           (O1 (nrm/exc #t #f))
           (O2 (nrm/exc #t #f))
           (l1 (if N (not i1) #f))
           (l2 (if O1 (or l1 i2) #f))
           (out (if O2 (or l2 i3) #f)))
        (list out N O1 O2)))))

; most probable explanations for faulty output #f given input #f/#f/#t 
(pr (circuit #f #f #t #f))