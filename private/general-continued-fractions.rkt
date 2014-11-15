#lang racket/base
(require "sequence-utils.rkt"
         )

(provide make-general-cf
         )

#|
General continued fractions are fractions of the form
a0 + b0/(a1 + b1/(a2 + ...))
They are most easily realized as the infinite matrix product over two
sequences, a_n and b_n,
 ∞
 Π [ [a_n, b_n], [1, 0] ]
n=0

The [1, 0] row vector seems to be a pointless addition, so it 
is better to utilize the associativity of square matrix multiplication 
and fold terms if possible. 
|#

(define (make-general-cf a b c d) ; [ [a, b], [c, d] ]
  (when (not (andmap sequence? (list a b c d)))
    (error 'make-general-cf "Expected four sequences: ~a"
           (car (filter not-sequence? (list a b c d)))))
  (define (entry->term entry)
    (caar entry))
  (define (position->terms pos)
    (map (λ(row) (map entry->term row)) pos))
  (define (entry->generator entry)
    (cdr entry))
  (define (position->generators pos)
    (map (λ(row) (map entry->generator row)) pos))
  (define (next-position current-position)
    (map (λ(row) (map (λ(generator) (values->pair (generator))) row))
         (position->generators current-position)))
  (define (continue? pos) ;die if ANY sequence dies, which it never should
    (andmap (λ(row) (andmap car row)) pos))
  (make-do-sequence
   (λ()
     (values position->terms
             next-position
             (list (list (values->pair (sequence-generate* a)) (values->pair (sequence-generate* b)))
                   (list (values->pair (sequence-generate* c)) (values->pair (sequence-generate* d))))
             continue? #f #f))))
#| SQUARE ROOTS
let √N = √(a^2 + b) = a + d
then
a^2 + b = a^2 + 2ad + d^2
b = 2ad + d^2 = d (2a + d)
so d = b/(2a + d) = b/(2a + b/(2a + ...))
then √N = a + d = a + b/(2a + b/(2a + ...))
which is 
                    ∞
 [ [a, b], [1, 0] ] Π [ [2a, b], [1, 0] ]
                   n=0
If we collapse pairs of the matrix product we get the repeating matrix product
                    ∞
 [ [a, b], [1, 0] ] Π [ [4a^2 + b, 2ab], [2a, b] ]
                   n=0
|#