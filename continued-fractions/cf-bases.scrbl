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
@(define this-eval (make-base-eval))
@interaction-eval[#:eval this-eval
                  (require "main.rkt"
                           "bases.rkt")]
@examples[#:eval this-eval
          (define p (make-representation #:radix #\_
                                         #:negate #\$
                                         #:terms "abcdefghij"))
          (parameterize ((representation p))
            (continued-fraction->string (rational->cf 11/3)))
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
