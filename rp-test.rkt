#lang racket

(require "main.rkt")

(module+ test

  (require rackunit)

  ; rf-equal as check
  (define-simple-check (check-rf-equal? r-exp1 r-exp2)
    (rf-equal? r-exp1 r-exp2))
  
  (test-case
   "nrm/exc"
   (check-rf-equal? (nrm 1 1 2)
                    (construct-ranking (1 . 0) (2 . 1)))
   (check-rf-equal? (nrm 1 2 2)
                    (construct-ranking (1 . 0) (2 . 2)))
   (check-rf-equal? (nrm 1 1 1)
                    (construct-ranking (1 . 0)))
   (check-rf-equal? (nrm 1 1 (nrm 2 1 3))
                    (construct-ranking (1 . 0) (2 . 1) (3 . 2)))
   (check-rf-equal? (nrm (nrm 10 5 20) 1 2)
                    (construct-ranking (10 . 0) (2 . 1) (20 . 5))))
  
  (test-case
   "either-of"
   (check-rf-equal? (either-of (list 1 2 3))
                    (construct-ranking (1 . 0) (2 . 0) (3 . 0)))
   (check-rf-equal? (either-of `())
                    (failure)))
  
  (test-case
   "either test"
   (check-rf-equal? (either)
                    (failure))
   (check-rf-equal? (either 1)
                    (construct-ranking (1 . 0)))
   (check-rf-equal? (either 1 2)
                    (construct-ranking (1 . 0) (2 . 0)))
   (check-rf-equal? (either (nrm 1 1 2) (nrm 10 10 20))
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
   "observe-l"
   (check-rf-equal? (observe-l odd? 100 (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (1 . 0) (3 . 2) (0 . 100) (2 . 102)))
   (check-rf-equal? (observe-l even? 100 (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (0 . 0) (2 . 2) (1 . 100) (3 . 102)))
   (check-rf-equal? (observe-l (lambda (x) #F) 100 (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
   (check-rf-equal? (observe-l (lambda (x) #T) 100 (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
                    (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3))))
  
   ;(test-case
   ;"observe-j"
   ;(check-rf-equal? (observe-j odd? 100 (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
   ;                 (construct-ranking (1 . 0) (3 . 2) (0 . 99) (2 . 101)))
   ;(check-rf-equal? (observe-j even? 100 (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
   ;                 (construct-ranking (0 . 0) (2 . 2) (1 . 100) (3 . 102)))
   ;(check-rf-equal? (observe-j (lambda (x) #F) 100 (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
   ;                 (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
   ;(check-rf-equal? (observe-j (lambda (x) #T) 100 (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3)))
   ;                 (construct-ranking (0 . 0) (1 . 1) (2 . 2) (3 . 3))))
   
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
   "@"
   (check-rf-equal? (@ + 10 5)
                    (construct-ranking (15 . 0)))
   (check-rf-equal? (@ (nrm + 1 -) 10 5)
                    (construct-ranking (15 . 0) (5 . 1)))
   (check-rf-equal? (@ + (nrm 10 1 20) 5)
                    (construct-ranking (15 . 0) (25 . 1)))
   (check-rf-equal? (@ + (nrm 10 1 20) (nrm 5 2 50))
                    (construct-ranking (15 . 0) (25 . 1) (60 . 2) (70 . 3)))
   (check-rf-equal? (@ (nrm + 1 -) (nrm 10 1 20) (nrm 5 2 50))
                    (construct-ranking (15 . 0) (25 . 1) (5 . 1) (60 . 2) (15 . 2) (70 . 3) (-40 . 3) (-30 . 4)))
   (check-rf-equal? (@ + (nrm 10 1 20) (nrm 5 2 (nrm 50 2 500)))
                    (construct-ranking (15 . 0) (25 . 1) (60 . 2) (70 . 3) (510 . 4) (520 . 5))))
  
  (test-case
   "rlet"
   (check-rf-equal? (rlet ((x (nrm 1 1 2))) x)
                    (construct-ranking (1 . 0) (2 . 1)))
   (check-rf-equal? (rlet ((x (nrm 1 1 2)) (y (nrm 10 2 20))) (list x y))
                    (construct-ranking ((1 10) . 0) ((2 10) . 1) ((1 20) . 2) ((2 20) . 3)))
   (check-rf-equal? (rlet ((x (nrm 1 1 2)) (y (failure))) (list x y))
                    (failure)))

  (test-case
   "rlet*"
   (check-rf-equal? (rlet* ((x (nrm 1 1 2))) x)
                    (construct-ranking (1 . 0) (2 . 1)))
   (check-rf-equal? (rlet* ((x (nrm #F 1 #T)) (y (if x (nrm 1 1 2) 0))) (list x y))
                    (construct-ranking ((#F 0) . 0) ((#T 1) . 1) ((#T 2) . 2)))
   (check-rf-equal? (rlet* ((x (nrm 1 1 2)) (y (failure))) (list x y))
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
     (check-equal? (rf->assoc (construct-ranking (1 . 0) (2 . 1) (2 . 2))) `((1 . 0) (2 . 1))))))
  
