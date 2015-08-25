#lang racket/base
(require "sequence-utils.rkt"
         "simple-matrix.rkt"
         )

; all procedures here are sequences which release matrix terms ((a b) (c d))
(provide make-general-cf general-cf
         make-simple-cf
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
and fold terms if possible. This may mean that convergents may
only approach from one direction.
|#

(define (general-cf a b c d #:force (times #f)) ; [ [a, b], [c, d] ]
  ; a term  is (t . generator) and the pass structure is ((term term) (term term))
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
  (define (force pos times)
    (let loop ((accum (position->terms pos))
               (pos pos)
               (times times))
      (if (zero? times)
          (let ((gens (position->generators pos)))
            (map (λ(row-terms row-gen) (map (λ(a b) (cons (list a) b)) row-terms row-gen))
                 accum gens))
          (let ((pos* (next-position pos)))
            (if (not (continue? pos*))
                (list accum) ; die if ANY sequence dies
                (let* ((T (position->terms pos*))
                       (a* (matrix* accum T)))
                  (loop a* pos* (sub1 times))))))))
  (make-do-sequence
   (λ()
     (values position->terms
             next-position
             (let ((A (values->pair (sequence-generate* a)))
                   (B (values->pair (sequence-generate* b)))
                   (C (values->pair (sequence-generate* c)))
                   (D (values->pair (sequence-generate* d))))
               (let ((start-pos `((,A ,B) (,C ,D))))
                 (if times
                     (force start-pos times)
                     start-pos)))
             continue? #f #f))))

(define (make-general-cf a b #:force (times #f))
  (general-cf a b (endless-values 1) (endless-values 0) #:force times))

(define (make-simple-cf terms)
  ; a term is (t . generator) and the pass structure is term
  (when (not (sequence? terms))
    (error 'make-simple-cf "Expected a sequence: ~a" terms))
  (define (continue? position)
    (car position))
  (define (position->terms position)
    (let ((t (caar position)))
      `((,t 1) (1 0))))
  (define (next-position position)
    (let ((generator (cdr position)))
      (values->pair (generator))))
  (make-do-sequence
   (λ()
     (values position->terms
             next-position
             (values->pair (sequence-generate* terms))
             continue? #f #f))))
