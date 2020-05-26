#lang racket

(require racket/match)
(provide (all-defined-out))
(require srfi/1)

;(define delay_x (lambda (x) (delay x)))

; return value captured (as value-promise) in given ranking element
(define value first)
(define rank second)
(define next-rank (lambda (x) (if (= (third x) -1) (rank (successor x)) (third x))))
(define successor-promise fourth)
(define successor (lambda (x) (force (successor-promise x))))

; Create new ranking element with given value-promise, rank and successor-promise
(define (element value rank next-rank successor-promise)
  ;(if (promise? value) (display (+ "a" "b"))
      (list value rank next-rank successor-promise));)

; Return element-promise that captures the terminal ranking element
(define terminate-promise
  (delay terminate-element))

; The terminal element (marks end of ranking function chain)
(define terminate-element
  (element 0 +inf.0 +inf.0 terminate-promise))

; map: create new ranking from given ranking by applying
; the given function to each value. The new ranking function
; contains the values returned by the function.
(define (map-value f rf)
   (delay
    (let ((res (force rf)))
      (if (infinite? (rank res))
          terminate-element
          (element
           (f (value res))
           (rank res)
           (next-rank res)
           (map-value f (successor-promise res)))))))

; normalise ranking
(define (normalise rf [s #f])
  (delay
    (let ((res (force rf)))
      (if (infinite? (rank res))
          terminate-element
          (element
           (value res)
           (if s (- (rank res) s) 0)
           (- (next-rank res) (if s s (rank res)))
           (normalise (successor-promise res)
                      (if s s (rank res))))))))

; shift rank
(define (shift n rf)
  (delay
    (let ((res (force rf)))
      (if (infinite? (rank res))
          terminate-element
          (element
           (value res)
           (+ (rank res) n)
           (+ (next-rank res) n)
           (shift n (successor-promise res)))))))

; filter on condition
(define (filter-ranking pred rf)
  (delay
    (let ((res (force rf)))
      (if (infinite? (rank res))
          terminate-element
          (if (pred (value res))
              (element
               (value res)
               (rank res)
               -1
               (filter-ranking pred (successor-promise res)))
              (force (filter-ranking pred (successor-promise res))))))))
              
; Only pass through first n elements
(define (filter-after n rf)
  (if (= n 0)
      terminate-promise
      (delay
        (let ((res (force rf)))
          (if (infinite? (rank res))
              terminate-element
              (element
               (value res)
               (rank res)
               (if (= n 1) +inf.0 (next-rank res))
               (filter-after (- n 1) (successor-promise res))))))))

; Only pass through elements with rank less than or equal to r
(define (up-to-rank r rf)
  (delay
    (let ((res (force rf)))
      (element
       (value res)
       (rank res)
       (if (> (next-rank res) r) +inf.0 (next-rank res))
       (if (> (next-rank res) r) terminate-element (up-to-rank r (successor-promise res)))))))

; For each element, call f with element as argument
(define (do-with rf f)
  (let ((res (force rf)))
    (when (not (infinite? (rank res)))
      (begin
        (f res)
        (do-with (successor-promise res) f)))))

; merge two ranking functions
(define (merge-shift rfa rfb rank)
  (merge* rfa
          (if (eq? rfa terminate-promise) +inf.0 0)
          (shift rank rfb)
          (if (eq? rfa terminate-promise) +inf.0 rank)))

; merge two ranking functions
(define (merge rfa rfb)
  (merge* rfa
          (if (eq? rfa terminate-promise) +inf.0 0)
          rfb
          (if (eq? rfb terminate-promise) +inf.0 0)))

; merge two ranking functions with ranks known
(define (merge* rfa ra rfb rb)
  (if (<= ra rb)
      (delay
        (let ((resa (force rfa)))
          (element
           (value resa)
           (rank resa)
           (min (next-rank resa) rb) ; todo: optimize? can we avoid calling next-rank?
           (merge* (successor-promise resa) (next-rank resa) rfb rb))))
      (delay
        (let ((resb (force rfb)))
          (element
           (value resb)
           (rank resb)
           (min (next-rank resb) ra) ; todo: optimize? 
           (merge* rfa ra (successor-promise resb) (next-rank resb)))))))       

; merge list of ranking functions (not used?)
;(define (merge-list rf-list)
;  (if (= (length rf-list) 1)
;      (car rf-list)
;      (merge
;       (car rf-list)
;       (merge-list (cdr rf-list)))))

; Join two ranking functions
(define (join rfa rfb)
  (merge-apply rfa (λ (va) (map-value (λ (b) (list va b)) rfb))))

; join list of ranking functions
(define (join-list rf-list)
  (cond
           [(empty? rf-list) terminate-promise]
           [(= (length rf-list) 1) (map-value list (car rf-list))]
           [(= (length rf-list) 2) (join (first rf-list) (second rf-list))]
           [else (map-value
                  (λ (x) (cons (first x) (second x)))
                  (join (first rf-list) (join-list (cdr rf-list))))]))

; merge-apply:
; - rfs is a ranking function
; - f is function taking one argument and returning a ranking function
; what is returned is the merge of all ranking functions returned by f,
; for each value returned by rfs, where ranks are increased accordingly
(define (merge-apply rfs f)
  (delay
    (let ((res (force rfs)))
      (if (infinite? (rank res))
          terminate-element
          (force (merge*
           (shift (rank res) (f (value res))) (rank res)
           (merge-apply (successor-promise res) f) (next-rank res)))))))

; check ranking correctness
(define (check-correctness rf [c -1])
  (delay
    (let ((res (force rf)))
      (if (and (infinite? (rank res)) (= c -1))
          res
          (if (<= c (rank res) (next-rank res))
              (element (value res)
                       (rank res)
                       (next-rank res)
                       (check-correctness (successor-promise res) (rank res)))
              (error "incorrect rank order" c (rank res) (next-rank res)))))))

; deduplicate ranking (remove repeated values with higher ranks)
(define (dedup rf [s (set)])
  (delay
    (let ((res (force rf)))
      (if (infinite? (rank res))
          terminate-element
          (if (set-member? s (value res))
              (force (dedup (successor-promise res) s))
              (element (value res)
                       (rank res)
                       -1
                       (dedup (successor-promise res) (set-add s (value res)))))))))

; convert ranking function to hash table (value->rank)
(define (to-hash rf)
  (letrec
      ((fill-hash
        (λ (rf table)
          (let ((res (force rf)))
            (when (not (infinite? (rank res)))
              (begin
                (when (not (hash-has-key? table (value res)))
                  (hash-set! table (value res) (rank res)))
                (fill-hash (successor-promise res) table))))))
       (tab (make-hash)))
    (fill-hash rf tab) tab))

; convert ranking function to associative list ((value . rank) (value . rank) ...)
(define (to-assoc rf)
  (let ((res (force rf)))
    (if (infinite? (rank res))
        `()
        (cons (cons (value res) (rank res)) (to-assoc (successor-promise res))))))

; convert ranking function to stream of pairs (value . rank)
(define (to-stream rf)
  (let ((res (force rf)))
    (if (infinite? (rank res))
        empty-stream
        (stream-cons (cons (value res) (rank res)) (to-stream (successor-promise res))))))

; convert associative list ((value . rank) (value . rank) ...) to ranking funciton
; (rank order is not checked)
(define (from-assoc assoc-list)
  (delay
    (if (empty? assoc-list)
        terminate-element
        (element
         (caar assoc-list)
         (cdar assoc-list)
         -1
         (from-assoc (cdr assoc-list))))))

; Some sanity checks
(define (test-eq a b)
  (when (not (equal? a b)) (error "fail")))

(test-eq (to-assoc
          (merge*
           (from-assoc (list `("a" . 0) `("b" . 1) `("c" . 3)))
           0
           (from-assoc (list `("A" . 0) `("B" . 2) `("C" . 4)))
           0))
         `(("a" . 0) ("A" . 0) ("b" . 1) ("B" . 2) ("c" . 3) ("C" . 4)))

(test-eq (to-assoc
          (shift 10 (from-assoc (list `("a" . 0) `("b" . 1) `("c" . 3)))))
         `(("a" . 10) ("b" . 11) ("c" . 13)))

(test-eq (to-assoc
          (map-value
           (lambda (x) (list x "x"))
           (from-assoc (list `("a" . 0) `("b" . 1) `("c" . 3)))))
         `((("a" "x") . 0) (("b" "x") . 1) (("c" "x") . 3)))

(test-eq (to-assoc
          (merge-apply
           (from-assoc (list `("a" . 0) `("b" . 1) `("c" . 3)))
           (lambda (x) (from-assoc (list (cons x 0) (cons (list x) 1))))))
         `(("a" . 0) (("a") . 1) ("b" . 1) (("b") . 2) ("c" . 3) (("c") . 4)))

(test-eq (to-assoc
          (join
           (from-assoc (list `("a" . 0) `("b" . 1) `("c" . 2)))
           (from-assoc (list `("A" . 0) `("B" . 1) `("C" . 2)))))
         `((("a" "A") . 0) (("a" "B") . 1) (("b" "A") . 1) (("a" "C") . 2) (("b" "B") . 2) (("c" "A") . 2) (("b" "C") . 3) (("c" "B") . 3) (("c" "C") . 4)))

;(pr (nrm/exc (nrm/exc 1 2 5) (nrm/exc 10 20) 3))
;;(to-assoc
;          (merge-shift
;           (from-assoc (list `(1 . 0) `(2 . 5)))
;           (from-assoc (list `(10 . 3) `(20 . 4)))
;           3))