#lang scribble/manual
@(require racket/sandbox
          scribble/example)

@require[@for-label[ranked-programming
                    racket/base]]
@(require (for-label racket/base
                     racket/contract/base
                     racket/struct-info
                     ranked-programming)
          ;scribble/extract
          ;scribble-math
          ;latex-utils/scribble/math
          ranked-programming)


@title{Ranked Programming}
@author{Tjitze Rienstra}

@defmodule[ranked-programming]

@section{Ranked Programming}

Ranked programming is ...

@subsection{Introduction}

This package implements extends Racket with @italic{ranked programming} functionality as described in (TODO).
There are some minor differences between the language described in that article, and the expressions implemented by this package.
Furthermore, several additional expression types not discussed in the paper, are included.

The differences are as follows:

@subsubsection{Truth expressions}

   The "truth expression" @racket[!x] is implemented by a procedure called @racket[!].
   This means that the correct expression is @racket[(! x)] and not @racket[!x] as in the paper.
   Thus, instead of writing

      @racket[(nrm !"foo" exc 1 !"bar")]

   as is done in the paper, one must write
   
      @racket[(nrm (! "foo") exc 1 (! "bar"))].

   @bold{However}, every expression with a parameter of type ranking also accepts values of any other type.
   Such values are implicitly converted to rankings.
   Therefore, the @racket[!] procedure is actually redundant, because one can also simply write

      @racket[(nrm "foo" exc 1 "bar")],

   where @racket["foo"] and @racket["bar"] are implicitly converted to @racket[(! "foo")] and @racket[(! "bar")],
   because they appear as arguments to parameters of type ranking.

@subsubsection{Ranking Functions}

   In this text, ranking functions returned by R-expressions are referred to as @italic{rankings}.
   They are represented by lazily-linked lists, as discussed in section 4 in the paper.
   For typical use-cases, the lazily-linked list structures used to represent rankings do not need to be processed manually.
   Instead, they should be used as input for procedures that either display the ranking
     (see @racket[pr], @racket[pr-all], @racket[pr-until] and @racket[pr-first])
   or convert them to some other, more manageable, representation
     (see @racket[rf->hash], @racket[rf->assoc] and @racket[rf->stream]).
   The @racket[cut] and @racket[limit] procedures may also be of use in combination with the
     procedures mentioned here.

   Note that forgetting to use @racket[pr] (or @racket[pr-all], @racket[pr-until] or @racket[pr-first])
     when evaluating an R-expression on the console leads to cryptic output.

   @examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
   (nrm "foo" exc "bar")
   ]

   The proper way is:

   @examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
   (pr (nrm "foo" exc "bar"))
   ]

   While the data structure used to represent a ranking never need to be processed manually
     in normal use-cases, one @italic{can} do so if desired.
   We refer to the description of @racket[ranking/c] for details on what these data structures look like.

@subsubsection{Additional expression types}

   A number of additional expression types that are not mentioned in the paper are implemented.
   These are (TODO).
   
 

In the paper, only the @racket[nrm/exc], @racket[either], 

@section{Important Implementation Details}

In the remainder of this text we will refer to ranking functions simply as @italic{rankings}.
The reason for this is that rankings are not actually represented by functions at all, and should
therefore not be confused with functions in Racket in the ordinary sense.


The marker symbol allows us to distinguish rankings from other objects,
 and the lazily linked list stores values and their associated ranks in increasing order with respect to rank.
The linked list is lazy in the sense that, to obtain the next value and rank, we need to call a function.

It is normally not
necessary to manipulate these objects directly. When we actually need the values and ranks for a given
ranking we use special functions included in this library for this purpuse (e.g. @racket[pr] to display
a ranking or @racket[rf->assoc] to convert a ranking to an associative list).

Nevertheless it is useful to have some understanding of what these special objects look like...

The parameters @racket[k1] and @racket[k2] are interpreted as rankings, even if the arguments that are used are not rankings.
Arguments that are not rankings are interpreted as rankings according to which the actual argument receives rank 0, and all other values receive rank infinity.
In the example above, the argument @racket["foo"] is interpreted as a ranking according to which @racket["foo"] receives rank 0 and all other values rank infinity,
  and similarly for @racket["bar"].


@section{Reference}

@defform[(nrm k_1 exc rank k_2)
         #:contracts ([k_1 (any/c)] [k_2 (any/c)] [rank (rank?)])]{

This expression @italic{normally} returns @racket[k_1] and @italic{exceptionally} (to degree @racket[rank]) @racket[k_2].
In other words, this expression returns a ranking where @racket[k_1] is receives rank 0 and @racket[k_2] receives rank @racket[rank].

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (nrm/exc "foo" "bar" 1))
]

The arguments @racket[k_1] and @racket[k_2] may also be rankings.
In this case, @racket[(nrm/exc k_1 k_2 rank)] returns a ranking
according to which the rank of a value @racket[v] is the minimum among:
@itemlist[@item{the rank of @racket[v] according to the ranking @racket[k_1],}
          @item{the rank of @racket[v] according to the ranking @racket[k_2] @bold{plus the value of} @racket[rank].}]
We can use this to construct more complex rankings.
The following expression normally returns @racket["foo"], and exceptionally (to degree @racket[1])
  returns a value that is again uncertain: normally @racket["bar"] and exceptionally (to degree @racket[2]) @racket["baz"].
Note that the rank with which @racket["baz"] is returned is the sum of @racket[1] and @racket[2].

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (nrm/exc "foo" (nrm/exc "bar" "baz" 2) 1))
]

Both @racket[k_1] and @racket[k_2] are evaluated on an as-needed basis. This means that
@racket[k_1] is evaluated only after the ranking that is returned is consulted for the first time,
and @racket[k_2] is evaluated only after the ranking that is returned is consulted beyond rank @racket[rank].
This @italic{lazy evaluation} scheme avoids needless calculations and provides the ability
  to define potentially infinite rankings.

The function @racket[recur] defined below is an example of an infinite ranking.
The expression @racket[(recur x)] normally returns @racket[x] and exceptionally (to degree 1) @racket[(recur (* x 2))].
Even though @racket[recur] is infinitely recursive, it does return a ranking, which is due to the lazy evaluation.
Note that @racket[pr] only displays the ten lowest-ranked values.
Using @racket[pr-all] instead of @racket[pr] in this example would lead to non-termination.

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(define (recur x) (nrm/exc x (recur (* x 2)) 1))
(pr (recur 1))
]
}

@defform[(nrm/exc k_1 k_2)
         #:contracts ([k_1 (any/c)] [k_2 (any/c)])]{

This is short for @racket[(nrm/exc k_1 k_2 1)].

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (nrm/exc "foo" "bar"))
]
}
                                                          
@defform[(either k_1 ... k_n)]{

Returns a ranking according to which @racket[k_1 ... k_n] all receive rank 0.
In other words, this expression returns either of the values of @racket[k_1 ... k_n], all equally likely.

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (nrm/exc "peter" (either "ann" "bob" "charlie")))
]

The arguments @racket[k_1 ... k_n] may also be rankings.
In this case, this expression returns a ranking
 according to which the rank of a value @racket[v] is the minimum among
  the ranks of @racket[v] according to the rankings @racket[k_1 ... k_n].

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (either (nrm/exc "peter" "ann") (nrm/exc "bob" "charly")))
]
}

@defproc[(either-of [lst (list?)])
         ranking?]{

Returns a ranking according to which the elements of @racket[lst] all receive rank 0.
In other words, this expression returns either of the elements of the list @racket[lst], all equally likely.

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(define weekdays `("mon" "tue" "wed" "thu" "fri"))
(define weekend `("sat" "sun"))
(pr (nrm/exc (either-of weekdays) (either-of weekend) 1))
]
}

@defproc[(rank-of [pred (any/c -> boolean?)] [k (ranking/c)])
         rank?]{
               
Returns the rank of the predicate @racket[pred] according to the ranking @racket[k].
This value represents the degree of surprise that @racket[pred] holds according to @racket[k].
It is the rank of the lowest-ranked value for which @racket[pred] returns @racket[#t].

If there is no finitely ranked value for which @racket[pred] returns @racket[#t] then what happens
depends on whether @racket[k] is an infinite ranking (i.e., assigns a finite rank to an infinite number of values).
If it is, then this procedure will not terminate, and if it is not, it will return infinity.
  
The following example determines the degree of surprise that @racket[(recur 1)] returns a value higher than 500.

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(define (recur x) (nrm/exc x (recur (* x 2)) 1))
(rank-of (lambda (x) (> x 500)) (recur 1))
]

}

The @racket[rank-of] procedure consults the ranking @racket[k] as much as necessary but no more.
More precisely, @racket[k] is consulted until a value for which @racket[pred] returns @racket[#t] is encountered.

@defproc[(observe [pred (any/c -> boolean?)] [k (ranking?)])
         ranking?]{

Returns the ranking @racket[k] conditionalized on the predicate @racket[pred].
This is the ranking-theoretic conditionalization operation,
  which is the ranking-based analogue of the probabilistic contitionalization operation.

The ranking returned by the expression @racket[(observe pred pred k)] is determined by the following rule:
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

@defproc[(ra [k_1 any/c] ... [k_n any/c])
         ranking?]{

Returns the result of applying the procedure @racket[k_1] to the arguments @racket[k_2 ... k_n].                                             

The precise rule that is used to construct the ranking returned by the expression @racket[(ra k_1 ... k_n)] is as follows:
Suppose that the rankings @racket[k_1 ... k_n] assign ranks @racket[r_1 ... r_n] to the values @racket[v_1 ... v_n].
Furthermore suppose that the standard procedure call @racket[(v_1 ... v_n)] returns @racket[v].
Then @racket[v] is returned with rank @racket[r_1]+...+@racket[r_n],
    unless there is another sequence of values that yields a lower rank for @racket[v] using the same rule.

If an argument for a parameter @racket[k_1 ... k_n] is not a ranking,
  then it is interpreted as a ranking according to which the
  given argument receives rank 0, and all other values rank infinity.
This is demonstrated by the following example.

Consider the procedure call @racket[(+ 5 10)].
The ranked version of this is @racket[(ra + 5 10)]:

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (ra + 5 10))
]

Now suppose we are uncertain about the argument @racket[10]:
 this value is normally @racket[10] and exceptionally @racket[20].
To express this we replace @racket[10] with @racket[(nrm/exc 10 20)]:

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (ra + 5 (nrm/exc 10 20)))
]

Now we add uncertainty about the operation: we normally add but exceptionally multiply:

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (ra (nrm/exc + *) 5 (nrm/exc 10 20)))
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
An example: let @racket[b] and @racket[p] be boolean variables standing for "beer" and "peanuts".
We only exceptionally drink beer, and we only exceptionally eat peanuts, and both are surprising to degree 1.
Thus, @racket[b] and @racket[p] both become @racket[(nrm/exc #f #t)].

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (rlet
     ((b (nrm/exc #f #t))
      (p (nrm/exc #f #t)))
     (string-append (if b "beer" "no beer") " and "
                    (if p "peanuts" "no peanuts"))))
]
}

One may wish to express dependencies between variables.
In the example above, we might wish to express that our peanut consumption depends on whether we drink beer.
However, this cannot be done, since the argument for @racket[p] cannot refer to the value of @racket[b].
The @racket[rlet*] expression extend @racket[rlet] and provides a solution for such cases.

@defform[(rlet* ([var_1 k_1] ... [var_n k_n]) body)]{

The @racket[rlet*] expression generalises Racket's standard @racket[let*] expression
  in a way similar to how @racket[rlet] generalises @racket[let]. 

The rule used to determine the ranking that is returned is the same as that of @racket[rlet],
  except that the expressions used as arguments for @racket[k_1 ... k_n] may refer to the preceding variables.
This provides a convenient way to construct a joint ranking over a list of variables,
where each variable may depend on the preceding variables.

An example: let @racket[b] and @racket[p] be boolean variables standing for "beer" and "peanuts".
We only exceptionally drink beer, and thus @racket[b] becomes @racket[(nrm/exc #f #t)].
However, our peanut consumption depends on whether we drink beer:
  if we have no beer, we have no peanuts, and if we have a beer, we normally have peanuts and exceptionally don't.
Thus, @racket[p] becomes @racket[(if b (nrm/exc #t #f) #f)].
Note that this expression refers to @racket[b], which would not be allowed if we used @racket[rlet] instead of @racket[rlet*].

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr (rlet* 
     ((b (nrm/exc #f #t))
      (p (if b (nrm/exc #t #f) #f)))
     (string-append (if b "beer" "no beer") " and "
                    (if p "peanuts" "no peanuts"))))
]
}

@defproc[(rf-equal? [k_1 (ranking/c)] [k_2 (ranking/c)])
         boolean?]{
Returns @racket[#t] if @racket[k_1] and @racket[k_2] are equivalent rankings, and @racket[#f] otherwise.
Two rankings are equivalent if they assign the same rank to each finitely-ranked value.
This means that the order in which values with the same rank are returned is irrelevant.
This procedure will not terminate if @racket[k_1] and @racket[k_2] are infinite rankings
  (i.e., assign finite rank to an infinite number of values).
  
}

@defproc[(rf->hash [k (ranking/c)])
         hash?]{
 Converts the ranking @racket[k] to a hash map that maps each finitely ranked
 value to its rank.
  This procedure will not terminate if @racket[k] is an infinite ranking
  (i.e., assigns finite rank to an infinite number of values).
}

@defproc[(rf->assoc [k (ranking/c)])
         list?]{
 Converts the ranking @racket[k] to an associative list consisting of pairs @racket[(value . rank)]
 for each finitely ranked value and its rank.
 These pairs appear in increasing order with respect to rank.
  This procedure will not terminate if @racket[k] is an infinite ranking
  (i.e., assigns finite rank to an infinite number of values).
}

@defproc[(rf->stream [k (ranking/c)])
         stream?]{
 Converts the ranking @racket[k] to a stream that generates pairs @racket[(value . rank)]
 for each finitely ranked value and its rank.
 These pairs are generated in increasing order with respect to rank.
}

@defproc[(pr-all [k (ranking/c)])
         void?]{
  Displays the complete ranking @racket[k] in tabular form and in increasing order with respect to rank.
  This procedure will not terminate if @racket[k] is an infinite ranking
  (i.e., assigns finite rank to an infinite number of values).
}

@defproc[(pr-until [rank (natural-number/c)] [k (ranking/c)])
         void?]{
  Like @racket[pr-all] but only displays values up to rank @racket[rank].
}

@defproc[(pr-first [n (natural-number/c)] [k (ranking/c)])
         void?]{
  Like @racket[pr-all] but only displays the @racket[n] lowest-ranked values.
}

@defproc[(pr [k (ranking/c)])
         void?]{
  Short for @racket[(pr-first 10 k)].
}

@defproc[(observe-l [pred (any/c -> boolean?)] [rank (rank?)] [k (ranking?)])
         ranking?]{

@margin-note{
Suppose @racket[k] assigns a finite rank @racket[r] to the value @racket[v]. Then:
@itemlist[
 @item{if @racket[(pred v)] returns @racket[#t] then @racket[v] is returned with rank @racket[r] minus @racket[(rank-of pred k)].}
 @item{if @racket[(pred v)] returns @racket[#f] then @racket[v] is returned with rank @racket[rank].}]
}

}

@defproc[(observe-j [pred (any/c -> boolean?)] [rank (rank?)] [k (ranking?)])
         ranking?]{

@margin-note{
Suppose @racket[k] assigns a finite rank @racket[r] to the value @racket[v]. Then:
@itemlist[
 @item{if @racket[(pred v)] returns @racket[#t] then @racket[v] is returned with rank @racket[r] minus @racket[(rank-of pred r)].}
 @item{if @racket[(pred v)] returns @racket[#f] then @racket[v] is returned with rank infinity (i.e., is not returned).}]
}
}

@defproc[(cut [rank (rank?)] [k (ranking?)])
         ranking?]{
Returns the ranking @racket[k] except that values with a rank higher than @racket[rank] are discarded.
}

@defproc[(ranking? [x (any/c)])
         boolean?]{

Returns @racket[#t] if @racket[x] is a ranking, @racket[#f] otherwise.
                   
}

@defthing[ranking/c flat-contract?]{

A contract that accepts rankings.}

@section{Parameters that expect rankings}

The functions and macros defined below return rankings and typically take one or more rankings as input through their parameters.
For these parameters we use the letter @racket[k], possibly with index (@racket[k_1], @racket[k_n]).
For convenience, however, these parameters also accept arguments that are not rankings.
This holds for @italic{all} parameters in the definitions below denoted with the letter @racket[k].

How this works is as follows: an argument for a @racket[k] parameter first goes through a type-check.
If the argument that is used is found to be a ranking, then this ranking will be used as the actual argument.
However, if the argument that is used is found @italic{not} to be a ranking, then it is converted to a ranking
  according to which the argument that is used receives rank 0, and all other values are ranked infinity.

The simplest example we can give is perhaps the @racket[pr] procedure, which has one ranking parameter @racket[k],
  and displays @racket[k] on the console in tabular form.
What we see below is that the non-ranking argument @racket["foo"] is interpreted as a ranking
  according to which @racket["foo"] receives rank 0, and all other values are ranked infinity.

@examples[ #:eval ((make-eval-factory #:lang 'racket/base
                             '(ranked-programming)))
(pr "foo")
]
  
