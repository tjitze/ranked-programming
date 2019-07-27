#lang racket
(require ranked-programming)

; Read dictionary
(define (read-file filename)
  (with-input-from-file filename
    (λ ()
      (for/list ([line (in-lines)])
        line))))

(define dictionary (map string->list (read-file "google-10000-english-no-swears.txt")))

; Below is an implementation of the "match" function mentioned in the paper that
; is based on a trie encoding of the dictionary, with lookup supporting wildcards.

; An empty trie
(define empty-trie (cons `() `()))

; Add key/value to trie
(define (add-to-trie trie key value)
  (cond
    [(empty? key) (cons (car trie) (list value))]
    [(assoc (car key) (car trie))
     (cons (dict-set (car trie) (car key) (add-to-trie (cdr (assoc (car key) (car trie))) (cdr key) value)) (cdr trie))] 
    [else (cons (dict-set (car trie) (car key) (add-to-trie empty-trie (cdr key) value)) (cdr trie))]))

; Add dictionary to tree (each word mapped to itself)
(define (add-all-to-trie trie dict)
  (if (empty? dict)  trie (add-to-trie (add-all-to-trie trie (cdr dict)) (car dict) (car dict))))

; Create the trie for the dictionary
(define dict-trie (add-all-to-trie empty-trie dictionary))

; Lookup value for given key in a set of tries
(define (lookup* tries word)
  (if (empty? word)
      (append* (map cdr tries))
      (if (equal? (car word) #\*)
          (lookup* (map cdr (append* (map car tries))) (cdr word))
          (lookup* (map cdr (filter (lambda (x) (equal? (car x) (car word))) (append* (map car tries)))) (cdr word)))))
 
; Take pattern as input, return list of strings that match pattern (* for wildcard)
(define (match pattern) (lookup* (list dict-trie) pattern))

; The gen function from the paper
(define (gen input)
  (if (empty? input)
      `()
      (nrm/exc ($ cons (car input) (gen (cdr input)))
               (either/or (gen (cdr input))
                          ($ cons #\* (gen (cdr input)))
                          ($ cons #\* (gen input))))))

; The correct function from the paper
(define (correct input)
  ($ match
     (observe
      (lambda (x) (not (empty? (match x))))
      (gen (string->list input)))))

; example
(pr-first 2 (correct "swtzerlandd"))

