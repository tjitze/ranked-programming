#lang racket

(require racket/match)
(require racket/format)
(require srfi/1)
(require "rp-core.rkt")
  
(provide
 ranking?
 rank?
 ranking/c
 rank/c
 construct-ranking
 for-ranking
 nrm/exc
 !
 failure
 either-of
 either/or
 observe
 deduplicate
 observe-r
 observe-e
 cut
 limit
 rank-of
 $
 rlet
 rlet*
 rf-equal?
 rf->hash
 rf->assoc
 rf->stream
 pr-all
 pr-until
 pr-first
 pr
 deepen-iteratively
)

; used as marker for ranking function (only for internal use)
(define rf-marker `ranking-function)

; autocast: if argument is ranking function, then return the ranking function, and
; if argument is any other value, return ranking function assigning rank 0 to that
; value and infinity to all other values.
(define-syntax autocast 
  (syntax-rules ()
    ((autocast value)
     (delay
       (if (ranking? value)
           (force (cdr value))
           (element value 0 +inf.0 terminate-promise))))))

; is value a ranking function?
(define (ranking? value) (and (pair? value) (eq? (car value) rf-marker)))

; ranking contract
(define ranking/c
  (flat-named-contract 'ranking/c ranking?))

; is value a rank?
(define (rank? x) (or (exact-nonnegative-integer? x) (infinite? x)))

; rank contract
(define rank/c
  (flat-named-contract 'rank/c rank?))

; is value a one-argument function? ()
(define (one-arg-proc? proc) (and (procedure? proc) (procedure-arity-includes? proc 1)))

; convert given ranking function to typed ranking function
(define (mark-as-rf rf) (cons rf-marker rf))

; empty ranking
(define terminate-rf (mark-as-rf terminate-promise))

; failure (returns empty ranking)
(define (failure) terminate-rf)

; construct ranking from associative pairs, e.g. (construct-ranking ("x" . 0) ("y" . 1))
(define-syntax construct-ranking 
  (syntax-rules ()
    ((construct-ranking a ...)
     (mark-as-rf
      (check-correctness
       (from-assoc `(a ...)))))))

; truth
(define !
  (lambda (value)
    (mark-as-rf
     (delay (element value 0 +inf.0 terminate-promise)))))

; for-ranking
(define-syntax for-ranking
  (syntax-rules ()
    ((for-ranking r exp) ($ (lambda (r) exp) r))))
           
; nrm/exc
(define-syntax nrm/exc
  (syntax-rules (nrm exc)
    ((nrm/exc r-exp1 r-exp2) (nrm/exc r-exp1 r-exp2 1))
    ((nrm/exc r-exp1 r-exp2 rank)
     (begin
       (unless (rank? rank) (raise-argument-error 'nrm/exc "rank (non-negative integer or infinity)" 1 r-exp1 rank r-exp2))
       (mark-as-rf
        (normalise
         (merge-shift (autocast r-exp1) (autocast r-exp2) rank)))))))

; either-of
(define (either-of lst)
  (unless (list? lst) (raise-argument-error 'either-of "list" 0 lst))
  (letrec
      ((either-of*
        (λ (list)
          (if (empty? list)
              terminate-promise
              (delay (element (car list) 0 (if (= (length list) 1) +inf.0 0) (either-of* (cdr list))))))))
    (mark-as-rf (either-of* lst))))

; either/or
(define-syntax either/or
  (syntax-rules ()
    ((either/or) (mark-as-rf terminate-promise))
    ((either/or r-exp rest ...) (mark-as-rf (merge (autocast r-exp) (either* rest ...))))))

(define-syntax either*
  (syntax-rules ()
    ((either*) terminate-promise)
    ((either* r-exp rest ...) (merge (autocast r-exp) (either* rest ...)))))

; observe
(define (observe pred r-exp)
  (begin
    (unless (one-arg-proc? pred) (raise-argument-error 'observe "predicate" 0 pred r-exp))
    (mark-as-rf (conditionalise-ranking pred (autocast r-exp)))))

; deduplicate ranking function
(define (deduplicate r-exp [cond #T])
  (if cond
      (mark-as-rf (dedup (autocast r-exp))) (autocast r-exp)))

; observe-r (result-oriented conditionalization, also called j-conditionalization)
(define (observe-r x pred r) 
  (unless (one-arg-proc? pred) (raise-argument-error 'observe-r "predicate" 0 pred x r))
  (unless (rank? x) (raise-argument-error 'observe-r "rank (non-negative integer or infinity)" 1 pred x r))
  (nrm/exc
   (observe pred r) 
   (observe (compose not pred) r) x))

; observe-e (evidence-oriented conditionalization, also called l-conditionalization)
(define (observe-e x pred r)
  (unless (one-arg-proc? pred) (raise-argument-error 'observe-e "predicate" 0 pred x r))
  (unless (rank? x) (raise-argument-error 'observe-e "rank (non-negative integer or infinity)" 1 pred x r))
  (let* ((rp (rank-of pred r)))
    (if (< x rp)
       (observe-r (- rp x) pred r)
       (observe-r (+ (- x rp) (rank-of (compose not pred) r)) pred r))))

; cut
(define (cut rank r-exp)
  (unless (rank? rank) (raise-argument-error 'cut "rank (non-negative integer or infinity)" 0 rank r-exp))
  (mark-as-rf (up-to-rank rank (autocast r-exp))))

; limit
(define (limit count r-exp)
  (unless (or (exact-nonnegative-integer? count) (infinite? count)) (raise-argument-error 'cut "non-negative integer" 0 rank r-exp))
  (mark-as-rf (filter-after count (autocast r-exp))))

; rank-of
(define (rank-of pred r-exp)
  (unless (one-arg-proc? pred) (raise-argument-error 'rank-of "predicate" 0 pred r-exp))
  (letrec ((rank-of*
            (λ (rf)
              (let* ((res (force rf)))
                (if (infinite? (rank res))
                    (rank res)
                    (if (pred (value res))
                        (rank res)
                        (rank-of* (successor-promise res))))))))
    (rank-of* (autocast r-exp))))

; $ (ranked application)
(define (apply-ranked cache r-exps)
  (if (> (length r-exps) 0)
      (if (ranking? (car r-exps))
          ; function argument is ranking over functions
          (mark-as-rf
           (merge-apply
             (map-value
              (λ (form) (autocast (apply-c cache (car form) (cdr form))))
              (join-list (map (lambda (x) (autocast x)) r-exps)))
             (λ (rf) rf)))
          (if (primitive? (car r-exps))
              ; function argument is primitive (function will not return ranking)
              (mark-as-rf
               (map-value
                (λ (args) (apply-c cache (car r-exps) args))
                (join-list (map (lambda (x) (autocast x)) (cdr r-exps)))))
              ; function argument is not primitive (function may return ranking) 
              (mark-as-rf
               (merge-apply
                (map-value
                 (λ (args) (autocast (apply-c cache (car r-exps) args)))
                 (join-list (map (lambda (x) (autocast x)) (cdr r-exps))))
                (λ (rf) rf)))))
      (raise-arity-error '$ (arity-at-least 1))))

(define ($ . r-exps) (apply-ranked #f r-exps))
(define ($@ . r-exps) (apply-ranked #t r-exps))

; ranked let
(define-syntax rlet
  (syntax-rules ()
    ((rlet ((var r-exp) ...) body ...)
      ($ (λ (var ...) body ...) r-exp ...))))

; ranked let*
(define-syntax rlet*
  (syntax-rules ()
    ((rlet* () body) body) ; base case
    ((rlet* ((var r-exp) rest ...) body) ; binding case
     ($ (λ (var) (rlet* (rest ...) body)) r-exp))
    ((rlet* ((@ var r-exp) rest ...) body) ; binding case
     ($@ (λ (var) (rlet* (rest ...) body)) r-exp))
    ))

; returns true if two ranking functions are equal (disregards ordering of values with equal rank and redundant elements)
(define (rf-equal? r-exp1 r-exp2)
  (equal? (rf->hash r-exp1) (rf->hash r-exp2)))

; rf->hash (convert ranking to hash table value->rank)
(define (rf->hash r-exp)
  (to-hash (autocast (deduplicate r-exp))))
  
; rf->assoc (convert ranking to associative list of (value . rank) pairs)
(define (rf->assoc r-exp)
  (to-assoc (autocast (deduplicate r-exp))))

; convert ranking function to stream of (value . rank) pairs
(define (rf->stream r-exp)
  (to-stream (autocast (deduplicate r-exp))))

; Display helper functions
(define display-header (λ () (begin (display "Rank  Value") (newline) (display "------------") (newline))))
(define display-failure (λ () (begin (display "Failure (empty ranking)") (newline))))
(define display-done (λ () (begin (display "Done") (newline))))
(define display-more (λ () (begin (display "...") (newline))))
(define display-element (λ (el) (begin (display (~a (rank el) #:min-width 5)) (display " ") (display (value el)) (newline))))

; Print complete ranking (todo: test this: autocast needs delay?)
(define (pr-all r-exp) 
  (unless (void? r-exp)
    (let ((first-res (force (autocast r-exp))))
      (if (infinite? (rank first-res))
          (display-failure)
          (begin (display-header) (do-with (delay first-res) display-element) (display-done))))))

; Print ranking for given r-expression up to given rank (todo: test this: autocast needs delay?)
(define (pr-until r r-exp)
  (unless (rank? r) (raise-argument-error 'observe-j "rank (non-negative integer or infinity)" 0 rank r-exp))
  (unless (void? r-exp)
    (let ((first-res (force (autocast r-exp))))
      (if (infinite? (rank first-res))
          (display-failure)
          (begin (display-header) (pr-until* r first-res))))))

(define (pr-until* r res)
  (if (infinite? (rank res))
      (display-done)
      (if (> (rank res) r)
          (display-more)
          (begin (display-element res) (pr-until* r (successor res))))))

; Print first n lowest-ranked values of ranking for given r-expression (todo: test this: autocast needs delay?)
(define (pr-first n r-exp)
  (unless (void? r-exp)
    (let ((first-res (force (autocast r-exp))))
      (if (infinite? (rank first-res))
          (display-failure)
          (begin (display-header) (pr-first* n first-res))))))

(define (pr-first* n res)
  (if (infinite? (rank res))
          (display-done)
          (begin (display-element res)
                 (if (= n 1) (display-more) (pr-first* (- n 1) (successor res))))))

; Print first 10 lowest-ranked values of ranking for given r-expression
(define (pr r-exp) (pr-first 10 r-exp))

(define (deepen-iteratively f args depth)
  (let ((res (force (autocast (apply f (append args (list depth)))))))
    (if (= (rank res) +inf.0)
        (begin (display "iter") (display depth) (newline)
               (deepen-iteratively f args (+ depth 100)))
        (mark-as-rf (delay res)))))
