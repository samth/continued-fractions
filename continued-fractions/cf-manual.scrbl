#lang scribble/manual
@(require scribble/eval
          (for-label racket
                     "main.rkt"
                     "bases.rkt"
                     )
          )
@(define (author-email) "deren.dohoda@gmail.com")

@title[#:tag "top"]{Elementary Continued Fraction Arithmetic Package}
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
rational and the value of the full continued fraction is less than @nonbreaking{d^2√5.}
This factor is a worst-case scenario, most continued fractions have convergents with much better
precision. In fact, the golden ratio @nonbreaking{φ = (1+√5)/2} is the worst case, constantly
staying just inside the bounds. Because of this, the golden ratio is sometimes called
the "most irrational" number; that is, the number that is hardest to approximate by rationals.

@subsection{The Arithmetic of Continued Fractions}
Continued fractions are amendable to term-at-a-time arithmetic. Suppose we have a function
@nonbreaking{f(x) = (ax+b)/(cx+d),} where x is a continued fraction of the form
@nonbreaking{x = t + 1/x',} r itself being the rest of the continued fraction. Then when
f consumes the leading term t, x goes to 1/x', which after multiplication to get f back
in the same form, has updated the coefficients. If the continued fraciton x is in standard form,
then we have guarantees about every term except the first nonzero term, namely, that it will lie
somewhere between 1 and infinity (a terminated continued fraction). Then if the limits of f
as x goes to 1 and as x goes to infinity agree, we can take a quotient term out of the function, and
f then goes to @nonbreaking{t + 1/f'.}

For further details, reference the MIT HAKMEM memo 239 where Dr Gosper outlines the details.

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
@defproc[(rational->cf [n exact?]) consumer-emitter?]{Creates a continued fraction representation of the
 argument.}
@defproc[(cf-terms->rational [cf (listof (and/c number? exact?))]) rational?]{Interprets a list of terms
 as a continued fraction expansion of a rational, and gives that rational.}
@defproc[(consumer-emitter? [v any/c]) boolean?]{Determines whether the argument is a continued fraction.}
@defproc[(phi-cf) consumer-emitter?]{A continued fraction of the golden ratio.}
@defproc[(pi-cf) consumer-emitter?]{A continued fraction of pi.}
@defproc[(exp-cf [n exact?]) consumer-emitter?]{A continued fraction of the natural exponential function.}
@defproc[(ln-cf [n exact?]) consumer-emitter?]{A continued fraction of the natural logarithm function.}
@defproc[(log-cf [b (and/c exact? integer? (>/c 1))]
                 [n exact?]) consumer-emitter?]{A continued fraction for the base-b logarithm function.}
@defproc[(sine-cf [n exact?]) consumer-emitter?]{}
@defproc[(cosine-cf [n exact?]) consumer-emitter?]{}
@defproc[(tangent-cf [n exact?]) consumer-emitter?]{Standard trigonometric functions.}
@defproc[(hyperbolic-sine-cf [n exact?]) consumer-emitter?]{}
@defproc[(hyperbolic-cosine-cf [n exact?]) consumer-emitter?]{}
@defproc[(hyperbolic-tangent-cf [n exact?]) consumer-emitter?]{Standard hyperbolic functions.}
@defproc[(expt-cf [base exact?]
                  [exponent exact?]) consumer-emitter?]{Raising exact numbers to exact rational powers.}

@subsection{Arithmetic}
@defproc[(cf+ [v (or/c exact-integer? consumer-emitter?)] ...)
         (or/c exact-integer? consumer-emitter?)]
@defproc[(cf- [v (or/c exact-integer? consumer-emitter?)] ...)
         (or/c exact-integer? consumer-emitter?)]
@defproc[(cf* [v (or/c exact-integer? consumer-emitter?)] ...)
         (or/c exact-integer? consumer-emitter?)]
@defproc[(cf/ [v (or/c exact-integer? consumer-emitter?)] ...)
         (or/c exact-integer? consumer-emitter?)]{Standard arithmetic procedures for continued fractions.}
@(define this-eval (make-base-eval))
@interaction-eval[#:eval this-eval
                         (require "main.rkt")]
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
@defproc[(cfpe (cf consumer-emitter?)) consumer-emitter?]
@defparam[precision n (and/c positive? number?)
          #:value (expt 2 30)]{Ordinarily, continued fractions are allowed to produce as many terms as
 requested. But this behavior can be controlled with the combined use
 of @racket[cfpe] and @racket[precision]. The @racket[precision] parameter
 represents the denominator in the error bound of the approximation
 (see @seclink["precision"]), so a value of @racket[100] means that
 the absolute value of the error between the continued fraction and
 the approximant yielded so far is less than @racket[1/100]. This value
 can be set to @racket[+inf.0], in which case the terms need to be stopped
 in some other way (e.g. through the use of @racket[(in-range 20)] in a
 for-clause). This is equivalent to pulling terms from the continued
 fraction itself.}
@interaction-eval[#:eval this-eval
                         (require (only-in racket/math pi))]
@examples[#:eval this-eval
          (define ~pi
            (parameterize ((precision 1000))
              (for/list ((t (cfpe (pi-cf))))
                t)))
          ~pi
          (abs (/ (- pi (cf-terms->rational ~pi))))
          (parameterize ((precision 3748630))
            (for/list ((t (cfpe (pi-cf))))
              t))
          (displayln
           (parameterize ((precision +inf.0))
             (for/list ((t (cfpe (pi-cf)))
                        (i (in-range 40)))
               t)))
          (displayln
           (for/list ((t (pi-cf))
                      (i (in-range 40)))
             t))]

@subsection{Emitting Terms in Specific Number Bases}
@defproc[(cfbe [v consumer-emitter?]
               [b (and/c exact-integer? (>/c 1))])
         consumer-emitter?]{Emits terms as if it were a base-b expansion rather than a
 continued fraction. The integer part does not
 respect the limits of the base as there is otherwise no way to determine
 where it would end, so this is mostly interesting for the
 fractional parts of numbers.
 
 The base emitter does not emit terms as a usual continued fraction where only the
 first non-zero term is allowed to be negative since that would change the value
 of the integer-part; instead, all terms are negative.}
@interaction-eval[#:eval this-eval
                         (require racket/math)]
@examples[#:eval this-eval
          (define phi (cf/ (cf+ 1 (expt-cf 5 1/2)) 2))
          phi
          (for/list ((t (cfbe phi 10))
                     (i (in-range 20)))
            t)
          (/ (+ 1 (sqrt 5)) 2)
          (for/list ((t (cfbe (rational->cf 1/10) 2))
                     (i (in-range 20)))
            t)
          (for/list ((t (cfbe (rational->cf 2/3) 3)))
            t)
          (for/list ((t (cfbe (rational->cf 101/3) 3))
                     (i (in-range 20)))
            t)
          (for/list ((t (cfbe (cf- (phi-cf) (exp-cf 1)) 10))
                     (i (in-range 20)))
            t)]

@section{Arbitrary Base Conversion}
@defmodule[continued-fractions/bases]
The bindings in this section are not imported with @racket[continued-fractions].
@defparam[representation v rep? #:value decimal-representation]
@defproc[(make-representation [#:radix rad (or/c #f char/c) #\.]
                              [#:negate neg char/c #\-]
                              [#:terms ts string? "0123456789"]) rep?]
This parameter controls the way string conversions happen. It contains information
about which symbols to use for which numbers, what symbol to use as a radix point, and
which symbol to use as a negative sign. It is intended to be constructed through
@racket[make-representation]. @racket[make-representation] checks that every part
of the string is unique and checks that the radix and negate characters differ from each
other and the string. The radix can be @racket[#f], in which case it is never displayed.
@defproc[(continued-fraction->string [cf (or/c consumer-emitter? (listof exact-integer?))])
         string?]
Converts a continued fraction, or a representation of one as a list of exact integers, into
a string based on the parameter @racket[representation].
@defparam[digits v (and/c number? exact? (not/c negative?))
          #:value 10]
Controls the number of fractional digits produced by @racket[continued-fraction->string].
@interaction-eval[#:eval this-eval
                         (require "bases.rkt")]
@examples[#:eval this-eval
          (define p (make-representation #:radix #\_
                                         #:negate #\$
                                         #:terms "abcdefghij"))
          (parameterize ((representation p))
            (continued-fraction->string (rational->cf 11/3)))
          (continued-fraction->string (cfbe (phi-cf) 10))]
