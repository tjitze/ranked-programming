#lang scribble/manual
@(require racket/sandbox
          scribble/example)

@require[@for-label[ranked-programming
                    racket/base]]
@(require (for-label racket/base
                     racket/contract/base
                     racket/struct-info
                     ranked-programming
                     racket/stream)
          ;scribble/extract
          ;scribble-math
          ;latex-utils/scribble/math
          ranked-programming)


@title{Ranked Programming}
@author{Tjitze Rienstra}

@defmodule[ranked-programming]

@section{Introduction}

The @racket[ranked-programming] package implements ranked programming functionality for the Racket programming language.
For background and general introduction on ranked programming please read
@(let ([url "https://github.com/tjitze/ranked-programming/blob/master/documentation/ranked_programming.pdf"])(link url "this paper")) (to be presented at IJCAI 2019).

A quick-start guide can be found @(let ([url "https://github.com/tjitze/ranked-programming/blob/master/README.md"]) (link url "here")). 
This document contains a complete reference of the functionality provided by this library.

Before using this reference, the reader should be familiar with the paper linked to above.
There are a few minor differences between the language described in the paper and the language implemented here,
  as well as a number of additional features not discussed in the paper.
We list them here:

@bold{Ranked Choice}

   The syntax of the @italic{ranked choice} expression discussed in the paper is

     @verbatim{(nrm K1 exc R K2)}

   The ranked choice expression implemented by this library uses a different syntax:

     @racket[(nrm/exc K1 K2 R)].

@bold{Either/Or}

   The syntax of the @italic{either/or} expression discussed in the paper is

     @verbatim{(either K1 or K2)}

   The either/or expression implemented by this library uses a different syntax:

     @racket[(either/or K1 K2)].

@bold{Truth expressions}

   The @italic{truth expression} @racket[!x] described in the paper is implemented by the procedure @racket[!].
   This means that we must enclose these expressions in parantheses.
   Thus, instead of writing

      @verbatim{(nrm/exc !"foo" !"bar" 1)}

   like in the paper, we have to write
   
      @racket[(nrm/exc (! "foo") (! "bar") 1)]

   @bold{However}, all expressions with parameters of type ranking are implemented
     so that these parameters also accept values of any other type.
   Such values are implicitly converted to rankings using @racket[!].
   Therefore, the @racket[!] procedure is actually redundant, because instead of @racket[(nrm/exc (! "foo") (! "bar") 1)] we can simply write

      @racket[(nrm/exc "foo" "bar" 1)]

   where @racket["foo"] and @racket["bar"] are implicitly converted to @racket[(! "foo")] and @racket[(! "bar")],
   since they appear as arguments to parameters of type ranking.

@bold{Displaying Ranking Functions}
   
   In this text, ranking functions returned by expressions are referred to simply as @italic{rankings}.
   They encode sets of possible return values of an expression, associated with degrees of surprise:
     0 for not surprising, 1 for surprising, 2 for even more surprising, and so on.
   These rankings are represented by lazily-linked list data structures, as discussed in section 4 in the
     @(let ([url "https://github.com/tjitze/ranked-programming/blob/master/documentation/ranked_programming.pdf"])
     (link url "paper")).
   In order to display a ranking, we need to provide it as an argument to one of the print functions implemented by this library.
   The standard print function is @racket[pr]. 
   Thus, instead of evaluating an expression like @racket[(nrm/exc "foo" "bar" 1)] directly, we must evaluate it as follows.

   @examples[ #:label #f #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
   (pr (nrm/exc "foo" "bar" 1))
   ]

   Alternatives to @racket[pr] are @racket[pr-all], @racket[pr-until] and @racket[pr-first] (see reference for details).

@bold{Doing other things with rankings}

   Apart from displaying a ranking, we can also convert it to some other, more manageable, representation.
   For this, we can use the @racket[rf->hash], @racket[rf->assoc] and @racket[rf->stream] functions,
     which convert a ranking to, respectively, a hash table, an association list, or a stream.
   The @racket[cut] and @racket[limit] procedures may also be of use in combination with these functions.

   @examples[ #:label "Example:" #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
   (rf->assoc (nrm/exc "foo" "bar"))
   ]

@bold{Additional functions and expression types}

   This library implements all functions and expression types discussed in the paper.
   These are the
     truth function (@racket[!]),
     ranked choice expression (@racket[nrm/exc]),
     the either/or syntactic shortcut (@racket[either/or]),
     observation function (@racket[observe]),
     ranked procedure call function (@racket[$]),
     and ranked @racket[let*] expression (@racket[rlet*]).

   This library implements a number of additional functions and expression types:

   @itemlist[
     @item{@racket[either-of] Choose elements from a list (all equally surprising).}
     @item{@racket[construct-ranking] Construct ranking from an association list.}
     @item{@racket[rank-of] Return rank of a predicate according to a given ranking.}
     @item{@racket[failure] Returns the empty ranking.}
     @item{@racket[rlet] Generalises @racket[let], like @racket[rlet*] generalises @racket[let*].}
     @item{@racket[rf-equal?] Check if two rankings are equivalent.}
     @item{@racket[rf->hash]/@racket[rf->assoc]/@racket[rf->stream] Convert ranking to other data structure.}
     @item{@racket[pr-all]/@racket[pr-first]/@racket[pr-until]/@racket[pr] Procedures for displaying a ranking.}
     @item{@racket[observe-l]/@racket[observe-j] Special @racket[observe] variants.}
     @item{@racket[cut] Restrict ranking up to a given rank.}
     @item{@racket[limit] Restrict ranking up to a given number of values.}
     @item{@racket[rank?]/@racket[ranking?] Type checking for ranks and rankings.}
     @item{@racket[rank/c]/@racket[ranking/c] Type contracts for ranks and rankings.}
   ]

   These are all described in detail in this reference.  

@section{Reference}

@defform*[((nrm/exc k_1 k_2 rank) (nrm/exc k_1 k_2))
         #:contracts ([k_1 (any/c)] [k_2 (any/c)] [rank (rank?)])]{

@italic{Normally} returns the value captured by @racket[k_1] and @italic{exceptionally} (with degree of surprise @racket[rank])
 the value captured by @racket[k_2].
If @racket[rank] is omitted, it defaults to 1.

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (nrm/exc "foo" "bar"))
(pr (nrm/exc "foo" "bar" 2))
]

If @racket[k_1] and @racket[k_2] are rankings then @racket[(nrm/exc k_1 k_2 rank)] returns a ranking
  according to which the rank of a value @racket[v] is the minimum among
  the rank of @racket[v] according to the ranking @racket[k_1],
  and the rank of @racket[v] according to the ranking @racket[k_2] @bold{plus the value of} @racket[rank].

@examples[ #:label #f #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (nrm/exc "foo" (nrm/exc "bar" "baz")))
]

Both @racket[k_1] and @racket[k_2] are evaluated on an as-needed basis. This means that
@racket[k_1] is evaluated only after the ranking that is returned is consulted for the first time,
  and @racket[k_2] only after it is consulted beyond rank @racket[rank].
This @italic{lazy evaluation} scheme avoids needless calculations and provides the ability
  to define potentially infinite rankings.

Below is an example of an infinite ranking.
The expression @racket[(recur x)] normally returns @racket[x] and exceptionally @racket[(recur (* x 2))].
Even though @racket[recur] is infinitely recursive, it does return a ranking, which is due to the lazy evaluation.

@examples[ #:label #f #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(define (recur x) (nrm/exc x (recur (* x 2))))
(pr (recur 1))
]
}
                                                          
@defform[(either/or k_1 ... k_n)]{

Returns a ranking according to which @racket[k_1 ... k_n] all equally surprising.

@examples[ #:label "Example:" #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (nrm/exc "peter" (either/or "ann" "bob" "charlie")))
]

If @racket[k_1 ... k_n] are rankings, then @racket[(either/or k_1 ... k_n)] returns a ranking
 according to which the rank of a value @racket[v] is the minimum among
  the ranks of @racket[v] according to the rankings @racket[k_1 ... k_n].

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (either/or (nrm/exc "peter" "ann") (nrm/exc "bob" "charly")))
]
}

@defproc[(either-of [lst (list?)])
         ranking?]{

Returns a ranking according to which all elements of @racket[lst] are equally surprising.

@examples[ #:label "Example:" #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(define weekdays `("mon" "tue" "wed" "thu" "fri"))
(define weekend `("sat" "sun"))
(pr (nrm/exc (either-of weekdays) (either-of weekend)))
]
}

@defform[(! v)]{

Constructs a ranking according to which @racket[v] is ranked 0 and anything else ranked infinity.

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (! 5))
]

This function (called the @italic{Truth function} in the paper) is included for the sake of completeness
but is actually redundant. This is because all expressions provided by this library with parameters of
type ranking are implemented so that these parameters also accept values of any other type. Such values
are implicitly converted to rankings using @racket[!]. See discussion in the introduction.}

@defform[(construct-ranking (v_1 . r_1) ... (v_n . r_n))]{

Constructs a ranking from an association list.
The values @racket[v_1] ... @racket[v_n] are returned with ranks @racket[r_1] ... @racket[r_n].
Rank @racket[r_1] must be 0, and @racket[r_1] ... @racket[r_n] must be sorted in non-decreasing order.

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (construct-ranking ("x" . 0) ("y" . 1) ("z" . 5)))
]
}

@defproc[(rank-of [pred (any/c -> boolean?)] [k (ranking/c)]) rank?]{
               
Returns the rank of the predicate @racket[pred] according to the ranking @racket[k].
This value represents the degree of surprise that @racket[pred] holds according to @racket[k].
It is the rank of the lowest-ranked value for which @racket[pred] returns @racket[#t].

If @racket[pred] does not return @racket[#t] for some finitely-ranked value,
   and @racket[k] is infinite (i.e., assigns finite ranks to infinitely many values)
   then @racket[rank-of] does not terminate.
  
The following example determines the degree of surprise that @racket[(recur 1)] returns a value higher than 500.

@examples[ #:label #f #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(define (recur x) (nrm/exc x (recur (* x 2)) 1))
(rank-of (lambda (x) (> x 500)) (recur 1))
]

The ranking @racket[k] is consulted as much as necessary but no more.
More precisely, @racket[k] is consulted until a value for which @racket[pred] returns @racket[#t] is encountered.}

@defproc[(failure) ranking?]{
Returns an empty ranking.
}

@defproc[(observe [pred (any/c -> boolean?)] [k (ranking?)])
         ranking?]{
Returns the ranking @racket[k] conditionalized on the predicate @racket[pred].
This is the ranking-theoretic conditionalization operation,
  which is the ranking-based analogue of the probabilistic contitionalization operation.

The ranking returned by the expression @racket[(observe pred k)] is determined by the following rule:
Suppose @racket[k] assigns a finite rank @racket[r] to the value @racket[v]. Then:
@itemlist[
 @item{if @racket[(pred v)] returns @racket[#f] then @racket[v] is discarded (or returned with rank infinity).}
 @item{if @racket[(pred v)] returns @racket[#t] then @racket[v] is returned with rank @racket[r] minus @racket[(rank-of pred k)].}]

In the following example we determine the ranking returned by @racket[(recur 1)] given that
we observe that the value that is returned is higher than 500.

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(define (recur x) (nrm/exc x (recur (* x 2)) 1))
(pr (observe (lambda (x) (> x 500)) (recur 1)))
]
}

@defproc[($ [k_1 any/c] ... [k_n any/c])
         ranking?]{

Returns the result of applying the procedure @racket[k_1] to the arguments @racket[k_2 ... k_n].                                             

The precise rule that is used to construct the ranking returned by the expression @racket[($ k_1 ... k_n)] is as follows:
Suppose that the rankings @racket[k_1 ... k_n] assign ranks @racket[r_1 ... r_n] to the values @racket[v_1 ... v_n].
Furthermore suppose that the standard procedure call @racket[(v_1 ... v_n)] returns @racket[v].
Then @racket[v] is returned with rank @racket[r_1]+...+@racket[r_n],
    unless there is another sequence of values that yields a lower rank for @racket[v] using the same rule.

If an argument for a parameter @racket[k_1 ... k_n] is not a ranking,
  then it is interpreted as a ranking according to which the
  given argument receives rank 0, and all other values rank infinity.
This is demonstrated by the following example.

Consider the procedure call @racket[(+ 5 10)].
The ranked version of this is @racket[($ + 5 10)]:

@examples[ #:label #f #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr ($ + 5 10))
]

Now suppose we are uncertain about the argument @racket[10]:
 this value is normally @racket[10] and exceptionally @racket[20].
To express this we replace @racket[10] with @racket[(nrm/exc 10 20)]:

@examples[ #:label #f #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr ($ + 5 (nrm/exc 10 20)))
]

Now we add uncertainty about the operation: we normally add but exceptionally multiply:

@examples[ #:label #f #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr ($ (nrm/exc + *) 5 (nrm/exc 10 20)))
]
}

@defform[(rlet ([var_1 k_1] ... [var_n k_n]) body)]{

The @racket[rlet] expression generalises Racket's standard @racket[let] expression.
Like in the standard @racket[let] expression, @racket[var_1 ... var_n] are variables,
  and @racket[body] is an expression in which these variables may occur.
The difference with the standard @racket[let] expression is that the @racket[k_1 ... k_n] parameters expect arguments of type ranking.

The precise rule that is used to construct the ranking returned by the expression @racket[(rlet ([var_1 k_1] ... [var_n k_n]) body)] is as follows:
Suppose that the rankings @racket[k_1 ... k_n] assign ranks @racket[r_1 ... r_n] to the values @racket[v_1 ... v_n].
Furthermore, suppose that @racket[body], with occurrences of @racket[var_1 ... var_n] replaced with @racket[v_1 ... v_n], returns @racket[v].
Then @racket[v] is returned with rank @racket[r_1]+...+@racket[r_n],
  unless there is another sequence of values that yields a lower rank for @racket[v] using the same rule.

The @racket[rlet] expression provides a convenient way to construct a joint ranking over a set of independent variables.
An example: let @racket[b] and @racket[p] be boolean variables standing for beer and peanuts.
We only exceptionally drink beer, and thus @racket[b] becomes @racket[(nrm/exc #f #t)].
Furthermore, we normally eat peanuts, and thus @racket[b] becomes @racket[(nrm/exc #t #f)].

@examples[ #:label #f #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (rlet
     ((b (nrm/exc #f #t))
      (p (nrm/exc #t #f)))
     (string-append (if b "beer" "no beer") " and "
                    (if p "peanuts" "no peanuts"))))
]
}

One may wish to express dependencies between variables.
In the example above, we might wish to express that our peanut consumption depends on whether we drink beer.
However, this cannot be done, since the argument for @racket[p] cannot refer to the value of @racket[b].
The @racket[rlet*] expression extends the @racket[rlet] expression and provides a solution for such cases.

@defform[(rlet* ([var_1 k_1] ... [var_n k_n]) body)]{

The @racket[rlet*] expression generalises Racket's standard @racket[let*] expression
  in a way similar to how @racket[rlet] generalises @racket[let]. 

The rule used to determine the ranking that is returned is the same as that of @racket[rlet],
  except that the expressions used as arguments for @racket[k_1 ... k_n] may refer to the preceding variables.
This provides a convenient way to construct a joint ranking over a list of variables,
where each variable may depend on the preceding variables.

An example: let @racket[b] and @racket[p] be boolean variables standing for "beer" and "peanuts".
Like before, we only exceptionally drink beer, and thus @racket[b] becomes @racket[(nrm/exc #f #t)].
However, this time our peanut consumption depends on whether we drink beer:
  if we do, we normally have peanuts, and otherwise we don't.
Thus, @racket[p] becomes @racket[(if b (nrm/exc #t #f) #f)].
Note that this expression refers to @racket[b],
  which would not be allowed if we used @racket[rlet] instead of @racket[rlet*].

@examples[ #:label #f #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (rlet* 
     ((b (nrm/exc #f #t))
      (p (if b (nrm/exc #t #f) #f)))
     (string-append (if b "beer" "no beer") " and "
                    (if p "peanuts" "no peanuts"))))
]
}

@defproc[(rf-equal? [k_1 (ranking/c)] [k_2 (ranking/c)]) boolean?]{
Returns @racket[#t] if @racket[k_1] and @racket[k_2] are equivalent rankings, and @racket[#f] otherwise.
Two rankings are equivalent if they assign the same rank to each finitely-ranked value.
This means that the order in which values with the same rank are returned is irrelevant.
This procedure will not terminate if @racket[k_1] and @racket[k_2] are infinite rankings
  (i.e., assign finite ranks to infinitely many values).
  
}

@defproc[(rf->hash [k (ranking/c)]) hash?]{
 Converts the ranking @racket[k] to a
  @(let ([url "https://docs.racket-lang.org/guide/hash-tables.html"])
   (link url "hash table")) that maps each finitely ranked value to its rank.
 This procedure will not terminate if @racket[k] is an infinite ranking
   (i.e., assigns finite ranks to infinitely many values).
}

@defproc[(rf->assoc [k (ranking/c)]) list?]{
 Converts the ranking @racket[k] to an association list,
   which is a list consisting of pairs @racket[(v . r)]
   for each finitely ranked value @racket[v] and rank @racket[r].
 These pairs appear in non-decreasing order with respect to rank.
 If @racket[k] is an infinite ranking (i.e., assigns finite ranks to infinitely many values) this function will not terminate.
}

@defproc[(rf->stream [k (ranking/c)]) stream?]{
 Converts the ranking @racket[k] to a stream that generates pairs @racket[(value . rank)]
   for each finitely ranked value and its rank (see @racket[racket/stream]).
 These pairs are generated in non-decreasing order with respect to rank.
 The ranking @racket[k] will be consulted one value at a time, as the stream is consumed.
 If @racket[k] is an infinite ranking (i.e., assigns finite ranks to infinitely many values) then this function returns an infinite stream.
}

@defproc[(pr-all [k (ranking/c)]) void?]{
 Displays the complete ranking @racket[k] in tabular form and in non-decreasing order with respect to rank.
 If @racket[k] is an infinite ranking (i.e., assigns finite ranks to infinitely many values) this function will not terminate.
}

@defproc[(pr-first [n (natural-number/c)] [k (ranking/c)]) void?]{
  Like @racket[pr-all] but only displays the @racket[n] lowest-ranked values.
}

@defproc[(pr-until [rank (natural-number/c)] [k (ranking/c)]) void?]{
  Like @racket[pr-all] but only displays values up to rank @racket[rank].
}

@defproc[(pr [k (ranking/c)]) void?]{
  Displayes the 10 lowest-ranked values of the ranking @racket[k].
  Short for @racket[(pr-first 10 k)].
}

@defproc[(observe-r [x (rank?)] [pred (any/c -> boolean?)] [k (ranking?)]) ranking?]{
Like @racket[observe] but implements the more general @italic{evidence-oriented} conditionalization operation,
where @racket[x] is the extra evidence strength parameter.}

@defproc[(observe-e [x (rank?)] [pred (any/c -> boolean?)] [k (ranking?)]) ranking?]{
Like @racket[observe] but implements the more general @italic{evidence-oriented} conditionalization operation,
where @racket[x] is the extra evidence strength parameter.}

@defproc[(cut [rank (rank?)] [k (ranking?)]) ranking?]{
Returns the ranking @racket[k] restricted to values with a rank of at most @racket[rank].
}

@defproc[(limit [count (rank?)] [k (ranking?)]) ranking?]{
Returns the ranking @racket[k] restricted to the @racket[count] lowest-ranked values.
}

@defproc[(rank? [x (any/c)]) boolean?]{
Returns @racket[#t] if @racket[x] is a rank (a non-negative integer or infinity) and @racket[#f] otherwise.
}

@defproc[(ranking? [x (any/c)]) boolean?]{
Returns @racket[#t] if @racket[x] is a ranking and @racket[#f] otherwise.
}

@defthing[rank/c flat-contract?]{
A contract for ranks (non-negative integers or infinity).
}

@defthing[ranking/c flat-contract?]{
A contract for rankings.
}

