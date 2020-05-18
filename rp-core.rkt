#lang racket

(require racket/match)
(provide (all-defined-out))
(require srfi/1)

; return value captured (as value-promise) in given ranking element
(define value (lambda (x) (force (car x))))

; return value-promise stored in given ranking element
(define value-promise car)

; return rank stored in given ranking element
(define rank cadr)

; return successor element-promise stored in given ranking element
(define successor-promise caddr)

; return successor ranking element stored (as element-promise) in given ranking element
(define successor (lambda (x) (force (caddr x))))

; Create new ranking element with given value-promise, rank and successor-promise
(define (element value-promise rank successor-promise)
  (list value-promise rank successor-promise))

; Return element-promise that captures the terminal ranking element
(define terminate-promise
  (delay terminate-element))

; The terminal element (marks end of ranking function chain)
(define terminate-element
  (element (delay 0) +inf.0 terminate-promise))

; map: create new ranking from given ranking by applying
; the given function to each value. The new ranking function
; contains the values returned by the function.
(define (map-value f rf)
   (delay
    (let ((res (force rf)))
      (if (infinite? (rank res))
          terminate-element
          (element
           (delay (f (value res)))
           (rank res)
           (map-value f (successor-promise res)))))))

; map-value-promise: like map-value, except that f takes
; a value promise as argument, and must return a value promise. 
(define (map-value-promise f rf)
   (delay
    (let ((res (force rf)))
      (if (infinite? (rank res))
          terminate-element
          (element
           (f (value-promise res))
           (rank res)
           (map-value-promise f (successor-promise res)))))))

; normalise ranking
(define (normalise rf [s #f])
  (delay
    (let ((res (force rf)))
      (if (infinite? (rank res))
          terminate-element
          (element
           (value-promise res)
           (if s (- (rank res) s) 0)
           (normalise (successor-promise res)
                      (if s s (rank res))))))))

; shift rank
(define (shift n rf)
  (delay
    (let ((res (force rf)))
      (if (infinite? (rank res))
          terminate-element
          (element
           (value-promise res)
           (+ (rank res) n)
           (shift n (successor-promise res)))))))

; filter on condition
(define (filter-ranking pred rf)
  (delay
    (let ((res (force rf)))
      (if (infinite? (rank res))
          terminate-element
          (if (pred (value res))
              (element
               (value-promise res)
               (rank res)
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
               (value-promise res)
               (rank res)
               (filter-after (- n 1) (successor-promise res))))))))

; Only pass through elements with rank less than or equal to r
(define (up-to-rank r rf)
  (delay
    (let ((res (force rf)))
      (if (> (rank res) r)
          terminate-element
          (element
           (value-promise res)
           (rank res)
           (up-to-rank r (successor-promise res)))))))

; For each element, call f with element as argument
(define (do-with rf f)
  (let ((res (force rf)))
    (when (not (infinite? (rank res)))
      (begin
        (f res)
        (do-with (successor-promise res) f)))))

; merge two ranking functions
(define (merge rfa rfb [sr 0])
   (delay
    (let ((resa (force rfa)))
      (if (= (rank resa) sr)
          (element
           (value-promise resa)
           (rank resa)
           (merge (successor-promise resa) rfb sr))
          (let ((resb (force rfb)))
            (if (<= (rank resa) (rank resb))
                (element
                 (value-promise resa)
                 (rank resa)
                 (merge (successor-promise resa) (delay resb) (rank resa)))
                (element
                 (value-promise resb)
                 (rank resb)
                 (merge (delay resa) (successor-promise resb) (rank resb)))))))))
         
; merge list of ranking functions
(define (merge-list rf-list)
  (if (= (length rf-list) 1)
      (car rf-list)
      (merge
       (car rf-list)
       (merge-list (cdr rf-list)))))

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
;(define (merge-apply rfs f)
;   (merge (shift (rank (force rfs)) (f (value (force rfs))))
;          (merge-apply (delay (successor (force rfs))) f)))
(define (merge-apply rfs f)
   (delay
    (let ((res (force rfs)))
      (if (infinite? (rank res))
          terminate-element
          (force
           (let ((res2 (successor res)))
             (merge-with-ranks
              (shift (rank res) (f (value res)))
              (rank res)
              (merge-apply (delay res2) f)
              (rank res2))))))))

; merge with ranks known
(define (merge-with-ranks rfa ra rfb rb)
  (cond
    [(and (infinite? ra) (infinite? rb)) terminate-promise]
    [(infinite? ra) rfb]
    [(infinite? rb) rfa]
    [(<= ra rb)
     (delay
       (let ((resa (force rfa)))
         (element
          (value-promise resa)
          (rank resa)
          (delay
            (let ((resa2 (successor resa)))
              (force (merge-with-ranks (delay resa2) (rank resa2) rfb rb)))))))]
     [else
      (delay
        (let ((resb (force rfb)))
          (element
           (value-promise resb)
           (rank resb)
           (delay
             (let ((resb2 (successor resb)))
               (force (merge-with-ranks rfa ra (delay resb2) (rank resb2))))))))]))

; check ranking correctness
(define (check-correctness rf [c -1])
  (delay
    (let ((res (force rf)))
      (if (and (infinite? (rank res)) (= c -1))
          res
          (if (>= (rank res) c)
              (element (value-promise res)
                       (rank res)
                       (check-correctness (successor-promise res) (rank res)))
              (error "incorrect rank order" c (rank res)))))))

; deduplicate ranking (removes duplicate higher-ranked values)
(define (dedup rf [s (set)])
  (if (not global-dedup-enabled)
      rf
      (delay
        (let ((res (force rf)))
          (if (infinite? (rank res))
              terminate-element
              (if (set-member? s (value res))
                  (force (dedup (successor-promise res) s))
                  (element (value-promise res)
                           (rank res)
                           (dedup (successor-promise res) (set-add s (value res))))))))))

; convert ranking function to hash table (value->rank)
(define (convert-to-hash rf)
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
(define (convert-to-assoc rf)
  (let ((res (force rf)))
    (if (infinite? (rank res))
        `()
        (cons (cons (value res) (rank res)) (convert-to-assoc (successor-promise res))))))

; convert ranking function to stream of pairs (value . rank)
(define (convert-to-stream rf)
  (let ((res (force rf)))
    (if (infinite? (rank res))
        empty-stream
        (stream-cons (cons (value res) (rank res)) (convert-to-stream (successor-promise res))))))

; convert associative list ((value . rank) (value . rank) ...) to ranking funciton
; (rank order is not checked)
(define (construct-from-assoc assoc-list)
  (delay
    (if (empty? assoc-list)
        terminate-element
        (element
         (delay (caar assoc-list))
         (cdar assoc-list)
         (construct-from-assoc (cdr assoc-list))))))

; Set the core deduplication setting: #T=enabled, #F=disabled (default=#T)
(define global-dedup-enabled #T)
(define (set-core-global-dedup x) (set! global-dedup-enabled x))
