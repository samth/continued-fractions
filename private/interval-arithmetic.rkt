#lang racket/base

(define (terms->interval l r)
  (define in/out (if (= (sgn l) (sgn r)) 'inside 'outside))
  (list in/out l r))

(define (inside? t)
  (eq? 'inside (car t)))

(define (union l r)
  (define L? (inside? l))
  (define R? (inside? r))
  (cond ((and L? R?)
         (let ((s (sign l)))