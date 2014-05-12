#lang racket/base

(provide sequences? not-sequence?
         endless-values enumerate-naturals
         interleave
         values->pair
         )

(define-syntax-rule (values->pair value-expression)
  (call-with-values (λ() value-expression) cons))

(define (sequences? . maybe-sequences)
  (andmap sequence? maybe-sequences))

(define (not-sequence? a)
  (not (sequence? a)))

(define (endless-values v)
  (define (id x) x)
  (make-do-sequence
   (λ() (values id
                id
                v
                #f #f #f))))

(define (enumerate-naturals from . action)
  (let ((proc (if (null? action) add1 (car action))))
    (make-do-sequence
     (λ() (values (λ(x) x)
                  (λ(x) (proc x))
                  from
                  #f #f #f)))))

(struct interleaver (todo done) ;lists
  #:property prop:sequence
  (λ(position)
    (define (renew pair)
      (define-values (s? s)
        ((cdr pair)))
      (and s? (cons s? s)))
    (make-do-sequence
     (λ()
       (values (λ(strct) ; position -> value
                 (caaar (interleaver-todo strct)))
               (λ(strct) ; position -> next position
                 (let ((todo (interleaver-todo strct))
                       (done (interleaver-done strct)))
                   (let ((next (renew (car todo))))
                     (if (null? (cdr todo))
                         (if next
                             (interleaver (reverse (cons next done)) null)
                             (interleaver (reverse done) null))
                         (let ((t (interleaver-todo strct)))
                           (if next
                               (interleaver (cdr t) (cons next done))
                               (interleaver (cdr t) done)))))))
               position
               (λ(strct) (not (null? (interleaver-todo strct))))
               #f
               #f)))))

(define (interleave . seqs)
  (interleaver (map (λ(s) (call-with-values (λ() (sequence-generate* s)) cons)) seqs) null))

