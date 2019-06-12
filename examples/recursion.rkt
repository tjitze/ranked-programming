#lang racket
(require ranked-programming)

; The function (fun x) normally returns x and exceptionally (fun (* x 2))
(define (fun x) (nrm/exc x (fun (* x 2))))

; We call (fun 1) and print the output. Note that (fun 1) yields an infinite 
; ranking (i.e. assigns finite ranks to infinitely many values) but that pr 
; only displays the 10 lowest ranked values. 
(pr (fun 1))

; Now we call (fun 1) and observe that the value is greater than 100.
(pr (observe (lambda (x) (> x 100)) (fun 1)))

