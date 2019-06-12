#lang racket
(require ranked-programming)

; We calculate the sum of two values
(pr ($ + 10 5))

; Now we add uncertainty: the first value is normally 10 but exceptionally 20
(pr ($ + (nrm/exc 10 20) 5))

; More uncertainty: we normally add, but exceptionally subtract
(pr ($ (nrm/exc + -) (nrm/exc 10 20) 5))

