#lang racket

(require "rp-api.rkt")
(require "rp-core.rkt")

(module+ test

  (require rackunit)

  ; rf-equal as check
  (define-simple-check (check-rf-equal? r-exp1 r-exp2)
    (rf-equal? r-exp1 r-exp2))
  
  (test-case
   "nrm/exc"
   (check-rf-equal? (nrm/exc 1 2 1)
                    (construct-ranking (1 . 0) (2 . 1)))
   (check-rf-equal? (nrm/exc 1 2 2)
                    (construct-ranking (1 . 0) (2 . 2)))
   (check-rf-equal? (nrm/exc 1 1 1)
                    (construct-ranking (1 . 0)))
   (check-rf-equal? (nrm/exc 1 (nrm/exc 2 3 1) 1)
                    (construct-ranking (1 . 0) (2 . 1) (3 . 2)))
   (check-rf-equal? (nrm/exc (nrm/exc 10 20 5) 2 1)
                    (construct-ranking (10 . 0) (2 . 1) (20 . 5))))

  ; This is to ensure that recursion in exceptional case does not lead to non-termination
  (test-case
   "nrm/exc infinite recursion"
   (check-rf-equal? (letrec ([fun (lambda (x)
                                    (nrm/exc x (fun (* 2 x))))])
                      (limit 4 (fun 1)))
                    (construct-ranking (1 . 0) (2 . 1) (4 . 2) (8 . 3))))
  
  (test-case
   "either-of"
   (check-rf-equal? (either-of (list 1 2 3))
                    (construct-ranking (1 . 0) (2 . 0) (3 . 0)))
   (check-rf-equal? (either-of `())
                    (failure)))
  
  (test-case
   "either/or test"
   (check-rf-equal? (either/or)
                    (failure))
   (check-rf-equal? (either/or 1)
                    (construct-ranking (1 . 0)))
   (check-rf-equal? (either/or 1 2)
                    (construct-ranking (1 . 0) (2 . 0)))
   (check-rf-equal? (either/or 1 2 3)
                    (construct-ranking (1 . 0) (2 . 0) (3 . 0)))
   (check-rf-equal? (either/or (nrm/exc 1 2 1) (nrm/exc 10 20 10))
                    (construct-ranking (1 . 0) (10 . 0) (2 . 1) (20 . 10))))
  
  (test-case
   "observe"
   (check-rf-equal? (observe odd? (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (1 . 0) (3 . 2)))
   (check-rf-equal? (observe even? (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (0 . 0) (2 . 2)))
   (check-rf-equal? (observe (lambda (x) #F) (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (failure))
   (check-rf-equal? (observe (lambda (x) #T) (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3))))
  
(test-case
   "observe-r"
   (check-rf-equal? (observe-r 100 odd? (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (1 . 0) (3 . 2) (0 . 100) (2 . 102)))
   (check-rf-equal? (observe-r 100 even? (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (0 . 0) (2 . 2) (1 . 100) (3 . 102)))
   ;(check-rf-equal? (observe-r 100 (lambda (x) #F) (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))  (TODO: fix this case)
   ;                 (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
   (check-rf-equal? (observe-r 100 (lambda (x) #T) (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3))))
  
(test-case
   "observe-e"
   (check-rf-equal? (observe-e 100 odd? (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (1 . 0) (3 . 2) (0 . 99) (2 . 101)))
   (check-rf-equal? (observe-e 100 even? (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (0 . 0) (2 . 2) (1 . 101) (3 . 103)))
   (check-rf-equal? (observe-e 100 (lambda (x) #F) (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (failure))
   (check-rf-equal? (observe-e 100 (lambda (x) #T) (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3))))
   
  (test-case
   "cut"
   (check-rf-equal? (cut 0 (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (0 . 0)))
   (check-rf-equal? (cut 1 (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (0 . 0) (1 . 1)))
   (check-rf-equal? (cut 3 (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3))))
  
  (test-case
   "rank-of"
   (check-equal? (rank-of odd? (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3))) 1)
   (check-equal? (rank-of even? (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3))) 0)
   (check-equal? (rank-of (lambda (x) (> x 2)) (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3))) 3)
   (check-true (infinite? (rank-of (lambda (x) #F) (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3))))))
  
  (test-case
   "ranked application"
   (check-rf-equal? ($ + 10 5)
                    (construct-ranking (15 . 0)))
   (check-rf-equal? ($ (nrm/exc + - 1) 10 5)
                    (construct-ranking (15 . 0) (5 . 1)))
   (check-rf-equal? ($ + (nrm/exc 10 20 1) 5)
                    (construct-ranking (15 . 0) (25 . 1)))
   (check-rf-equal? ($ + (nrm/exc 10 20 1) (nrm/exc 5 50 2))
                    (construct-ranking (15 . 0) (25 . 1) (60 . 2) (70 . 3)))
   (check-rf-equal? ($ (nrm/exc + - 1) (nrm/exc 10 20 1) (nrm/exc 5 50 2))
                    (construct-ranking (15 . 0) (25 . 1) (5 . 1) (60 . 2) (15 . 2) (70 . 3) (-40 . 3) (-30 . 4)))
   (check-rf-equal? ($ + (nrm/exc 10 20 1) (nrm/exc 5 (nrm/exc 50 500 2) 2))
                    (construct-ranking (15 . 0) (25 . 1) (60 . 2) (70 . 3) (510 . 4) (520 . 5))))
  
  (test-case
   "rlet"
   (check-rf-equal? (rlet ((x (nrm/exc 1 2 1))) x)
                    (construct-ranking (1 . 0) (2 . 1)))
   (check-rf-equal? (rlet ((x (nrm/exc 1 2 1)) (y (nrm/exc 10 20 2))) (list x y))
                    (construct-ranking ((1 10) . 0) ((2 10) . 1) ((1 20) . 2) ((2 20) . 3)))
   (check-rf-equal? (rlet ((x (nrm/exc 1 2 1)) (y (failure))) (list x y))
                    (failure)))

  (test-case
   "rlet*"
   (check-rf-equal? (rlet* ((x (nrm/exc 1 2 1))) x)
                    (construct-ranking (1 . 0) (2 . 1)))
   (check-rf-equal? (rlet* ((x (nrm/exc #F #T 1)) (y (if x (nrm/exc 1 2 1) 0))) (list x y))
                    (construct-ranking ((#F 0) . 0) ((#T 1) . 1) ((#T 2) . 2)))
   (check-rf-equal? (rlet* ((x (nrm/exc 1 2 1)) (y (failure))) (list x y))
                    (failure)))
  
  (test-case
   "rf-equal"
   (check-true (rf-equal? (failure) (failure)))
   (check-true (rf-equal? (construct-ranking (0 . 0)) (construct-ranking (0 . 0))))
   (check-true (rf-equal? (construct-ranking (0 . 0) (1 . 1)) (construct-ranking (0 . 0) (1 . 1))))
   (check-false (rf-equal? (construct-ranking (0 . 0) (1 . 1)) (construct-ranking (0 . 0) (1 . 2))))
   (check-false (rf-equal? (construct-ranking (0 . 0) (1 . 1)) (construct-ranking (0 . 0) (2 . 1))))
   (check-false (rf-equal? (construct-ranking (0 . 0) (1 . 1)) (construct-ranking (0 . 0))))
   (check-false (rf-equal? (construct-ranking (0 . 0)) (construct-ranking (0 . 0) (1 . 1))))
   (check-false (rf-equal? (construct-ranking (0 . 0)) (failure))))
  
  (test-case
   "rf->hash"
   (let ((hash1 (rf->hash (failure)))
         (hash2 (rf->hash (construct-ranking (1 . 0))))
         (hash3 (rf->hash (construct-ranking (1 . 0) (2 . 1))))
         (hash4 (rf->hash (construct-ranking (1 . 0) (2 . 1) (2 . 2)))))
     (check-equal? (hash-count hash1) 0)
     (check-equal? (hash-count hash2) 1)
     (check-equal? (hash-count hash3) 2)
     (check-equal? (hash-count hash4) 2)
     (check-equal? (hash-ref hash2 1) 0)
     (check-equal? (hash-ref hash3 1) 0)
     (check-equal? (hash-ref hash3 2) 1)
     (check-equal? (hash-ref hash4 1) 0)
     (check-equal? (hash-ref hash4 2) 1)))
  
  (test-case
   "rf->assoc"
   (let ((r1 (failure))
         (r2 (construct-ranking (1 . 0)))
         (r3 (construct-ranking (1 . 0) (2 . 1))))
     (check-equal? (rf->assoc (failure)) `())
     (check-equal? (rf->assoc (construct-ranking (1 . 0))) `((1 . 0)))
     (check-equal? (rf->assoc (construct-ranking (1 . 0) (2 . 1))) `((1 . 0) (2 . 1)))
     (check-equal? (rf->assoc (construct-ranking (1 . 0) (2 . 1) (2 . 2))) `((1 . 0) (2 . 1)))))

  (test-case
   "merge"
   (check-equal? (to-assoc
                  (merge*
                   (from-assoc (list `("a" . 0) `("b" . 1) `("c" . 3)))
                   0
                   (from-assoc (list `("A" . 0) `("B" . 2) `("C" . 4)))
                   0))
                 `(("a" . 0) ("A" . 0) ("b" . 1) ("B" . 2) ("c" . 3) ("C" . 4))))

  (test-case
   "shift"
   (check-equal? (to-assoc
                  (shift 10 (from-assoc (list `("a" . 0) `("b" . 1) `("c" . 3)))))
                 `(("a" . 10) ("b" . 11) ("c" . 13))))

  (test-case
   "map-value"
   (check-equal? (to-assoc
          (map-value
           (lambda (x) (list x "x"))
           (from-assoc (list `("a" . 0) `("b" . 1) `("c" . 3)))))
         `((("a" "x") . 0) (("b" "x") . 1) (("c" "x") . 3))))

  (test-case
   "merge-apply"
   (check-equal? (to-assoc
           (merge-apply
           (from-assoc (list `("a" . 0) `("b" . 1) `("c" . 3)))
           (lambda (x) (from-assoc (list (cons x 0) (cons (list x) 1))))))
         `(("a" . 0) (("a") . 1) ("b" . 1) (("b") . 2) ("c" . 3) (("c") . 4))))

  (test-case
   "join"
   (check-equal? (to-assoc
          (join
           (from-assoc (list `("a" . 0) `("b" . 1) `("c" . 2)))
           (from-assoc (list `("A" . 0) `("B" . 1) `("C" . 2)))))
         `((("a" "A") . 0) (("a" "B") . 1) (("b" "A") . 1) (("a" "C") . 2) (("b" "B") . 2) (("c" "A") . 2) (("b" "C") . 3) (("c" "B") . 3) (("c" "C") . 4))))

)
