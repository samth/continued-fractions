#lang scribble/manual
@(require scribble/eval
          (for-label racket
                     "main.rkt"
                     )
          )

@(define (author-email) "deren.dohoda@gmail.com")

@title[#:tag "cf-top"]{Elementary Continued Fraction Arithmetic Package}
@author{@(author+email "Deren Dohoda" (author-email))}

@defmodule[continued-fractions]

@table-of-contents[]

@section{Concepts}
@subsection{Introduction}
A simple continued fraction is a number of the form a+1/(b+1/(c+...)), possibly infinite.
They have the convenient property that terminating the expression at some point results
in a rational that is "close" to the whole expression, in the sense that there is no
rational closer with a smaller denominator. For instance,
@nonbreaking{14/3 = 4 + 2/3 = 4 + 1/(3/2) = 4 + 1/(1 + 1/2).} When all the numerators are
1 like this, we can simply write @nonbreaking{(4 1 2).} This process will always terminate
for rational numbers since it is equivalent to the Euclidean algorithm.

Real solutions to quadratic equations with integer coefficients have repeating continued
fractions. For example, @nonbreaking{√2 = (1; 2 ...)} which is
@nonbreaking{1 + 1/(2 + 1/(2 + ...))}. Other algebraics do not have a repeating
simple continued fraction.

Simple continued fractions are unique, given a few caveats. As you can see
from the 14/3 example, @nonbreaking{4 + 1/(1 + 1/2) = 4 + 1/(1 + 1/(1 + 1))}, which is to say
that @nonbreaking{(a b ... c)} is the same as @nonbreaking{(a b ... c-1 1).}
Additionally, interspersed zeroes can be put in such that
@nonbreaking{(a b 0 n c ...)} is the same as @nonbreaking{(a b+n c ...).} Lastly, the representation
of negative numbers is not unique, either. If @bold{B} is a [possibly infinite]
continued fraction @nonbreaking{(a b c ...)} then there are two ways to write -@bold{B}. The first
is @nonbreaking{(-a -b -c ...)} and the second is @nonbreaking{(-(a+1) 1 |b|-1 |c| ...).}
This library prefers the second form.

Many common functions or numbers have a continued fraction representation where the numerators
are not all 1s, such as the continued fraction for pi which can be written
@nonbreaking{0 + 4/(1 + 1^2/(3 + 2^2/(5 + 3^2/(7 + ...)))).} Such representations are not unique.

@subsection[#:tag "precision"]{Precision}
When considered as a regular continued fraction, terminating the continued fraction at some point
yields a rational n/d, called a "convergent." Then the absolute value of the difference between this
rational and the value of the full continued fraction is less than one divided the product of the
denominators of this convergent and the next best convergent.
This factor is a worst-case scenario, most continued fractions have convergents with much better
precision. In fact, the golden ratio @nonbreaking{φ = (1+√5)/2} is the worst case, constantly
staying just inside the bounds. Because of this, the golden ratio is sometimes called
the "most irrational" number; that is, the number that is hardest to approximate by rationals.

@subsection{The Arithmetic of Continued Fractions}
Continued fractions are amendable to term-at-a-time arithmetic. Suppose we have a function
@nonbreaking{f(x) = (ax+b)/(cx+d),} where x is a continued fraction of the form
@nonbreaking{x = t + 1/x',} x' being the rest of the continued fraction. Then when
f consumes the leading term t, x goes to @nonbreaking{t+1/x',} which after multiplication to get f back
in the same form, has updated the coefficients. If the continued fraciton x is in standard form,
then we have guarantees about every term except the first nonzero term, namely, that it will lie
somewhere between 1 and infinity (a terminated continued fraction). Then if the limits of f
as x goes to 1 and as x goes to infinity agree it does not matter what the value of x' is as
the leading term of f is already determined. We can take a quotient term out of the function, and
f then goes to @nonbreaking{t + 1/f'.}

For further details, reference the MIT HAKMEM memo 239 (Gosper).

@subsection{Purpose}
Because of the term-at-a-time aspect of continued fraction arithmetic, calculating terms and
quotients proceeds without unnecessary calculation. Because of the precision guarantees,
calculation can be terminated when the desired precision is exceeded. Combining these facts
give us a picture of exact arithmetic of rationals and irrationals which only calculate
as far as needed and yield the rational with the smallest denominator possible for the
given precision.

@section{Use}
@subsection{Provided Continued Fractions}
All of the continued fractions provided are sequences. If you want to see the terms of the simple
continued fraction generated, then the standard use should be something like
@racketblock[(for/list ((term (exp-cf 1/2))
                        (i (in-range 20)))
               term)]
It is important to have a limiting sequence included in the for-clause because the provided continued
fraction procedures of transcendental and algebraic functions tend to produce an infinite number of terms.
@defproc[(rational->cf [n exact?]) continued-fraction?]{Creates a continued fraction representation of the
 argument.}
@defproc[(cf-terms->rational [cf (listof (and/c number? exact?))]) rational?]{Interprets a list of terms
 as a continued fraction expansion of a rational, and gives that rational.}
@defproc[(continued-fraction? [v any/c]) boolean?]{Determines whether the argument is a continued fraction.}
@defproc[(phi-cf) continued-fraction?]{A continued fraction of the golden ratio.}
@defproc[(pi-cf) continued-fraction?]{A continued fraction of pi.}
@defproc[(exp-cf [n exact?]) continued-fraction?]{A continued fraction of the natural exponential function.}
@defproc[(ln-cf [n exact?]) continued-fraction?]{A continued fraction of the natural logarithm function.}
@defproc[(log-cf [b (and/c exact? integer? (>/c 1))]
                 [n exact?]) continued-fraction?]{A continued fraction for the base-b logarithm function.}
@defproc[(sine-cf [n exact?]) continued-fraction?]{}
@defproc[(cosine-cf [n exact?]) continued-fraction?]{}
@defproc[(tangent-cf [n exact?]) continued-fraction?]{Standard trigonometric functions.}
@defproc[(hyperbolic-sine-cf [n exact?]) continued-fraction?]{}
@defproc[(hyperbolic-cosine-cf [n exact?]) continued-fraction?]{}
@defproc[(hyperbolic-tangent-cf [n exact?]) continued-fraction?]{Standard hyperbolic functions.}
@defproc[(expt-cf [base exact?]
                  [exponent exact?]) continued-fraction?]{Raising exact numbers to exact rational powers.}

@subsection{Arithmetic Procedures}
@defproc[(cf+ [v (or/c exact-integer? continued-fraction?)] ...)
         (or/c exact-integer? continued-fraction?)]
@defproc[(cf- [v (or/c exact-integer? continued-fraction?)] ...)
         (or/c exact-integer? continued-fraction?)]
@defproc[(cf* [v (or/c exact-integer? continued-fraction?)] ...)
         (or/c exact-integer? continued-fraction?)]
@defproc[(cf/ [v (or/c exact-integer? continued-fraction?)] ...)
         (or/c exact-integer? continued-fraction?)]{Standard arithmetic procedures for continued fractions.}
@(define this-eval (make-base-eval))
@interaction-eval[#:eval this-eval
                  (require (only-in racket/math pi)
                           "main.rkt")]
@examples[#:eval this-eval
          (for/list ((t (cf/ (pi-cf) (phi-cf) (expt-cf 2 1/2)))
                     (i (in-range 20)))
            t)]

@subsection{Controlling Behavior}
There are two ways provided to control the behavior of continued fractions. The first is to limit the number
of terms continued fractions are allowed to consume while they attempt to produce the next output term.
@defparam[consume-limit n (and/c exact? positive? integer?)
          #:value 500]{Every continued fraction has internal sequences it continues when
 it needs to generate the next term. In order to generate a term, the internal sequence may be continued
 many times, up to this limit. This parameter is the limit used. It is reset for every term the continued
 fraction generates. If the  limit is reached then:
 @itemlist[@item{A message is sent to the @racket[current-error-port] indicating the fact.}
           @item{The current state is coerced to a rational and the terms of this rational
             become the rest of the sequence.}
           #:style 'ordered]}
@defproc[(precision-emit (cf continued-fraction?)) continued-fraction?]
@defparam[precision n (and/c positive? number?)
          #:value (expt 2 30)]{Ordinarily, continued fractions are allowed to produce as many terms as
 requested. But this behavior can be controlled with the combined use
 of @racket[precision-emit] and @racket[precision]. The @racket[precision] parameter
 represents the denominator in the error bound of the approximation
 (see @seclink["precision"]), so a value of @racket[100] means that
 the absolute value of the error between the continued fraction and
 the approximant yielded so far is less than @racket[1/100]. This value
 can be set to @racket[+inf.0], in which case the terms need to be stopped
 in some other way (e.g. through the use of @racket[(in-range 20)] in a
 for-clause). This is equivalent to pulling terms from the continued
 fraction itself.}
@examples[#:eval this-eval
          (define ~pi
            (parameterize ((precision 1000))
              (for/list ((t (precision-emit (pi-cf))))
                t)))
          ~pi
          (abs (/ (- pi (cf-terms->rational ~pi))))
          (parameterize ((precision 3748630))
            (for/list ((t (precision-emit (pi-cf))))
              t))
          (displayln
           (parameterize ((precision +inf.0))
             (for/list ((t (precision-emit (pi-cf)))
                        (i (in-range 40)))
               t)))
          (displayln
           (for/list ((t (pi-cf))
                      (i (in-range 40)))
             t))]

@subsection{Arithmetic Caveats}
There is no way, in general, to determine whether an infinite continued fraction is exactly a rational number;
particularly whether it is zero. And there is no way to know whether some arithmetic expression is zero, so
therefore there is no way to determine if we are dividing by zero. But continued fraction arithmetic is
a little more lenient in that division by zero is "possible" in the degenerate sense that the empty continued
fraction is infinity. (For a justification of this, please see "Exact Real Computer Arithmetic with
Continued Fractions," Vuillemin (1988).) Thus, division by zero in this library will not produce errors but
instead empty continued fractions.
@examples[#:eval this-eval
          (for/list ((t (cf/ (exp-cf 1) (cf- (phi-cf) (phi-cf))))
                     (i (in-range 10)))
            t)
          (for/list ((t (cf/ (exp-cf 1) (cf- 2 2)))
                     (i (in-range 10)))
            t)
          
          (for/list ((t (cf/ (exp-cf 1) (rational->cf 0)))
                     (i (in-range 10)))
            t)
          (for/list ((t (cf/ (rational->cf 0) (rational->cf 0))))
            t)]
Expressions which yield exact rationals from infinite continued fractions have their own problems. It would
take an infinite number of terms to yield the rational but at any one time we've only ever taken a
finite number of them.
@examples[#:eval this-eval
          (let ((sqrt (λ(n) (expt-cf n 1/2))))
            (for/list ((t (cf* (sqrt 2) (sqrt 2)))
                       (i (in-range 10)))
              t))
          (for/list ((t (cf- (expt-cf 2 1/2) (expt-cf 2 1/2)))
                     (i (in-range 10)))
            t)]
It would not matter how far we increased the consume limit as we could never increase it to infinity. In
the case of the arithmetic of two continued fractions, if neither continued fraction has run out of terms
by the time @racket[consume-limit] is reached then it rounds the internal rational. Thus, in the calculation
of @racket[(cf* (sqrt 2) (sqrt 2))] above the guess made by the library is @racket['(2)] rather than
something like @racket['(2 10000000000000000000000)] or @racket['(1 1 99999999999999999999999)]. 

@subsection{Emitting Terms in Specific Number Bases}
@defproc[(base-emit [v continued-fraction?]
               [b (and/c exact-integer? (>/c 1))])
         continued-fraction?]{Emits terms as if it were a base-b expansion rather than a
 continued fraction. The integer part does not respect the limits of the base as there is otherwise no
 way to determine where it would end, so this is mostly interesting for the fractional parts of numbers.
 
 The base emitter does not emit terms as a usual continued fraction where only the
 first non-zero term is allowed to be negative since that would change the value
 of the integer-part; instead, all terms are negative.}
@examples[#:eval this-eval
          (define phi (cf/ (cf+ 1 (expt-cf 5 1/2)) 2))
          (for/list ((t (base-emit phi 10))
                     (i (in-range 20)))
            t)
          (/ (+ 1 (sqrt 5)) 2)
          (for/list ((t (base-emit (rational->cf 1/10) 2))
                     (i (in-range 20)))
            t)
          (for/list ((t (base-emit (rational->cf 2/3) 3)))
            t)
          (for/list ((t (base-emit (rational->cf 101/3) 3))
                     (i (in-range 20)))
            t)
          (for/list ((t (base-emit (cf- (phi-cf) (exp-cf 1)) 10))
                     (i (in-range 20)))
            t)]

@include-section["cf-bases.scrbl"]
