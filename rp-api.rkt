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
 nrm/exc
 !
 failure
 either-of
 either
 observe
 observe-l
 observe-j
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
)

; used as marker for ranking function (only for internal use)
(define rf-marker `ranking-function)

; autocast: convert typed ranking function to ranking chain
; and any other value to ranking chain representing single-valued ranking function
(define autocast
  (lambda (value)
    (if (ranking? value)
        (force (cdr value))
        (element (delay value) 0 terminate-promise))))

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
       (construct-from-assoc `(a ...)))))))

; truth
(define !
  (lambda (value)
    (mark-as-rf
     (element (delay value) 0 terminate-promise))))

; nrm (alternative syntax)
(define-syntax nrm/exc
  (syntax-rules (nrm exc)
    ((nrm/exc r-exp1 r-exp2) (nrm/exc r-exp1 r-exp2 1))
    ((nrm/exc r-exp1 r-exp2 rank)
     (begin
       (unless (rank? rank) (raise-argument-error 'nrm/exc "rank (non-negative integer or infinity)" 1 r-exp1 rank r-exp2))
       (mark-as-rf
        (dedup
         (normalise
          (merge
           (delay (autocast r-exp1))
           (shift rank (delay (autocast r-exp2)))))))))))

; either-of
(define (either-of lst)
  (unless (list? lst) (raise-argument-error 'either-of "list" 0 lst))
  (letrec
      ((either-of*
        (λ (list)
          (if (empty? list)
              terminate-promise
              (delay (element (delay (car list)) 0 (either-of* (cdr list))))))))
    (mark-as-rf (dedup (either-of* lst)))))

; either
(define-syntax either
  (syntax-rules (or)
    ((either) (mark-as-rf terminate-promise))
    ((either r-exp1 or r-exp2) (either r-exp1 r-exp2))
    ((either r-exp1 or r-exp2 or r-exp3) (either r-exp1 r-exp2 r-exp3))
    ((either r-exp1 or r-exp2 or r-exp3 or r-exp4) (either r-exp1 r-exp2 r-exp3 r-exp4))
    ((either r-exp1 or r-exp2 or r-exp3 or r-exp4 or r-exp5) (either r-exp1 r-exp2 r-exp3 r-exp4 r-exp5))
    ((either r-exp rest ...) (mark-as-rf (dedup (merge (delay (autocast r-exp)) (either* rest ...)))))))

(define-syntax either*
  (syntax-rules ()
    ((either*) terminate-promise)
    ((either* r-exp rest ...) (merge (delay (autocast r-exp)) (either* rest ...)))))

; observe
(define (observe pred r)
  (begin
    (unless (one-arg-proc? pred) (raise-argument-error 'observe "predicate" 0 pred r))
    (mark-as-rf (normalise (filter-ranking pred (delay (autocast r)))))))

; observe-l
(define (observe-l pred rank r) 
  (unless (one-arg-proc? pred) (raise-argument-error 'observe-l "predicate" 0 pred rank r))
  (unless (rank? rank) (raise-argument-error 'observe-l "rank (non-negative integer or infinity)" 1 pred rank r))
  (nrm/exc
   (observe pred r) 
   (observe (compose not pred) r)
   rank))

; observe-j
(define (observe-j pred rank r)
  (unless (one-arg-proc? pred) (raise-argument-error 'observe-j "predicate" 0 pred rank r))
  (unless (rank? rank) (raise-argument-error 'observe-j "rank (non-negative integer or infinity)" 1 pred rank r))
  (let* ((rank-pred (rank-of pred r))
         (rank-not-pred (rank-of (compose not pred) r)))
    (if (<= rank-pred rank)
        (nrm/exc
         (observe pred r)
         (observe (compose not pred) r)
         (+ (- rank rank-pred) rank-not-pred))
        (nrm/exc
         (observe (compose not pred) r)
         (observe pred r)
         (- rank-pred rank)))))

; cut
(define (cut rank r-exp)
  (unless (rank? rank) (raise-argument-error 'cut "rank (non-negative integer or infinity)" 0 rank r-exp))
  (mark-as-rf (up-to-rank rank (delay (autocast r-exp)))))

; limit
(define (limit count r-exp)
  (unless (or (exact-nonnegative-integer? count) (infinite? count)) (raise-argument-error 'cut "non-negative integer" 0 rank r-exp))
  (mark-as-rf (filter-after count (delay (autocast r-exp)))))

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
    (rank-of* (delay (autocast r-exp)))))

; $ (ranked application)
(define ($ . r-exps)
  (if (> (length r-exps) 0)
      (if (ranking? (car r-exps))
          ; function argument is ranking over functions
          (mark-as-rf
           (dedup
            (merge-apply
             (map-value
              (λ (form) (delay (autocast (apply (car form) (cdr form)))))
              (dedup (join-list (map (λ (x) (delay (autocast x))) r-exps))))
             (λ (rf) rf))))
          (if (primitive? (car r-exps))
              ; function argument is primitive (function will not return ranking)
              (mark-as-rf
               (dedup
                (map-value
                 (λ (args) (apply (car r-exps) args))
                 (dedup (join-list (map (λ (x) (delay (autocast x))) (cdr r-exps)))))))
              ; function argument is not primitive (function may return ranking)
              (mark-as-rf
               (dedup
                (merge-apply
                 (map-value
                  (λ (args) (delay (autocast (apply (car r-exps) args))))
                  (dedup (join-list (map (λ (x) (delay (autocast x))) (cdr r-exps)))))
                 (λ (rf) rf))))))
      (raise-arity-error '$ (arity-at-least 1))))

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
      ($ (λ (var) (rlet* (rest ...) body)) r-exp))))

; returns true if two ranking functions are equal (disregards ordering of values with equal rank and redundant elements)
(define (rf-equal? r-exp1 r-exp2)
  (equal? (rf->hash r-exp1) (rf->hash r-exp2)))

; rf->hash (convert ranking to hash table value->rank)
(define (rf->hash r-exp)
  (convert-to-hash (delay (autocast r-exp))))
  
; rf->assoc (convert ranking to associative list of (value . rank) pairs)
(define (rf->assoc r-exp)
  (convert-to-assoc (dedup (delay (autocast r-exp)))))

; convert ranking function to stream of (value . rank) pairs
(define (rf->stream r-exp)
  (convert-to-stream (dedup (delay (autocast r-exp)))))

; Display helper functions
(define display-header (λ () (begin (display "Rank  Value") (newline) (display "------------") (newline))))
(define display-failure (λ () (begin (display "Failure (empty ranking)") (newline))))
(define display-done (λ () (begin (display "Done") (newline))))
(define display-more (λ () (begin (display "...") (newline))))
(define display-element (λ (el) (begin (display (~a (rank el) #:min-width 5)) (display " ") (display (value el)) (newline))))

; Print complete ranking
(define (pr-all r-exp)
  (unless (void? r-exp)
    (let ((first-res (force (autocast r-exp))))
      (if (infinite? (rank first-res))
          (display-failure)
          (begin (display-header) (do-with (delay first-res) display-element) (display-done))))))

; Print ranking for given r-expression up to given rank
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

; Print first n lowest-ranked values of ranking for given r-expression
(define (pr-first n r-exp)
  (unless (void? r-exp)
    (let ((first-res (force (autocast r-exp))))
      (if (infinite? (rank first-res))
          (display-failure)
          (begin (display-header) (pr-first* n first-res))))))

(define (pr-first* n res)
  (if (infinite? (rank res))
          (display-done)
          (if (= n 0)
              (display-more)
              (begin (display-element res) (pr-first* (- n 1) (successor res))))))

; Print first 10 lowest-ranked values of ranking for given r-expression
(define (pr r-exp) (pr-first 10 r-exp))