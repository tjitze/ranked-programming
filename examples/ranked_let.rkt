#lang racket
(require ranked-programming)

; Let p and b be boolean variables standing for "beer" and "peanuts". We
; only exceptionally drink beer, and thus p becomes:
;
;   (nrm/exc #f #t). 
;
; Our peanut consumption depends on whether we drink beer: if we do, we 
; normally have peanuts, otherwise we don't. Thus b becomes:
; 
;   (if b (nrm/exc #t #f) #f).
;
; Note that this expression refers to b (p is dependent on b). We can 
; express this with a ranked let expression.
(pr (rlet*
  ((b (nrm/exc #f #t))
   (p (if b (nrm/exc #t #f) #f)))
  (list "beer:" b "peanuts:" p)))

