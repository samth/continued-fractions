#lang scribble/manual
@(require scribble/eval
          (for-label racket
                     "main.rkt"
                     "bases.rkt"
                     )
          )
@(define (author-email) "deren.dohoda@gmail.com")

@title[#:tag "cf-bases-top"]{Base Conversion Package}
@author{@(author+email "Deren Dohoda" (author-email))}

@defmodule[continued-fractions/bases]

@table-of-contents[]

@section{Arbitrary Base Conversion}
@subsection{Configuration}
The bindings in this section are not imported with @racket[continued-fractions].
@defproc[(rep? [v any/c]) boolean?]
@defparam[representation v rep? #:value decimal-representation]
@defproc[(make-representation [#:radix rad (or/c #f char?) #\.]
                              [#:negate neg char? #\-]
                              [#:terms ts string? "0123456789"]) rep?]
This parameter controls the way string conversions happen in either direction.
It contains information
about which symbols to use for which numbers, what symbol to use as a radix point, and
which symbol to use as a negative sign. The magnitude of the base is determined from the
number of characters in the string specified by @racket[#:term]. The first character of
the terms is considered to be zero,
the second character is considered to be one, and so on. The parameter is intended to be
constructed through @racket[make-representation]. @racket[make-representation] checks
that every part of the string is unique and checks that the radix and negate characters
differ from each other and the string. The radix can be @racket[#f], in which case it is
never displayed.
@defparam[digits v (and/c number? exact? (not/c negative?))
          #:value 10]
Controls the number of fractional digits produced.

@subsection{Converting Continued Fractions}
@defproc[(continued-fraction->string [cf (or/c continued-fraction? (listof exact-integer?))])
         string?]
Converts a continued fraction, or a representation of one as a list of exact integers, into
a string based on the parameter @racket[representation].
@(define this-eval (make-base-eval))
@interaction-eval[#:eval this-eval
                  (require "main.rkt" "bases.rkt")]
@examples[#:eval this-eval
          (define p (make-representation #:radix #\_
                                         #:negate #\$
                                         #:terms "abcdefghij"))
          (parameterize ((representation p))
            (continued-fraction->string (rational->cf 11/3)))
          (parameterize ((representation
                          (make-representation #:terms "01"))
                         (digits 20))
            (continued-fraction->string (tangent-cf #e3.14)))
          (parameterize ((digits 20))
            (continued-fraction->string (phi-cf)))
          (define ~phi
            (for/list ((t (cfbe (phi-cf) 10))
                       (i (in-range 40)))
              t))
          (continued-fraction->string ~phi)
          (parameterize ((digits 20))
            @code:comment{note this won't work because it interprets}
            @code:comment{the terms in the continued fraction as a}
            @code:comment{simple continued fraction, but this isn't}
            @code:comment{a simple continued fraction, it's decimal}
            (continued-fraction->string (cfbe (phi-cf) 10)))
          ]

@subsection{Converting Numbers and Strings}
@defproc[(->string [n exact?]) (or/c string? boolean?)]{Converts an exact
 number to a string based on @racket[representation]. Trailing zeroes and leading
 zeroes are not printed. If the argument is not an exact number, the expression
 evaluates to @racket[#f].}
@defproc[(->number [s string?]) (or/c exact? boolean?)]{Interprets a
 string as a number based on @racket[representation]. The parameter @racket[digits] is
 not respected when reading strings, only when writing them. If the argument is not
 a string, the expression evaluates to @racket[#f].}
@examples[#:eval this-eval
          (->number "0.1234556789101112")
          (->number "0.123455678910111213")
          (->string #e0.123455678910111213)]
@defproc[(string->string [s string?]
                         [rep* rep?])
         (or/c string? boolean?)]{Converts a string with the current value of
 @racket[representation] to a number, then to a string based on the @racket[rep*]
 argument. The interpretation of the string doesn't respect the @racket[digits]
 parameter, but the subsequent output of the string with the new @racket[representation]
 does.}
@examples[#:eval this-eval
          (define alphabet* (make-representation #:radix #\,
                                                 #:terms " abcdefghijklmnopqrstuvwxyz!"))
          (->number "123.456")
          (string->string "123.456" alphabet*)
          (string->string "123.456789011121314" (representation))
          (parameterize ((digits 15))
            (string->string "123.456789011121314" (representation)))
          (parameterize ((representation alphabet*)
                         (digits 0))
            (->string 9395845/17210367))
          (parameterize ((representation alphabet*)
                         (digits 20))
            (->string 263083660/481890303))
          (parameterize ((representation alphabet*)
                         (digits 34))
            (->string 1169629299742001994435343/232218265089212416))]
