#lang racket/base
(require "private/arithmetic.rkt"
         "private/continued-fractions.rkt")

(provide (all-from-out "private/arithmetic.rkt")
         phi-cf pi-cf
         exp-cf
         ln-cf log-cf
         sine-cf cosine-cf tangent-cf
         hyperbolic-sine-cf hyperbolic-cosine-cf hyperbolic-tangent-cf
         expt-cf
         rational->cf cf-terms->rational
         precision cfbe cfpe
         )