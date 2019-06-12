#lang racket
(require ranked-programming)

; the ranking network example from the paper
(define (network)
 (rlet*
  ((H (nrm/exc #f #t 15))
   (B (if H (nrm/exc #f #t 4)
            (nrm/exc #t #f 8)))
   (F (nrm/exc #t #f 10))
   (S (cond [(and B F) (nrm/exc #t #f 3)]
            [(and B (not F)) (nrm/exc #f #t 13)]
            [(and (not B) F) (nrm/exc #f #t 11)]
            [else (nrm/exc #f #t 27)])))
  (list H B F S)))

; evaluate complete network
(pr (network))

; ranking over S given that F is true
(pr ($ fourth (observe third (network))))