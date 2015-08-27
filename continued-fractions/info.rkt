#lang info
(define scribblings '(("cf-manual.scrbl" (multi-page))))
(define deps (list "base"
                   "math-lib"))
(define build-deps (list "racket-doc"
                         "rackunit-lib"
                         "scribble-lib"))

