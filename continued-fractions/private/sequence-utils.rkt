#lang racket/base

(require racket/generic
         )

(provide (all-defined-out)
         #|sequences? not-sequence?
         endless-values enumerate-naturals
         interleave
         values->pair
         pass-sequence ;|#
         )

(define-syntax-rule (values->pair value-expression)
  (call-with-values (λ() value-expression) cons))

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
  (let ((proc (if (null? action) (λ(x) x) (car action))))
    (make-do-sequence
     (λ() (values (λ(x) (proc x))
                  (λ(x) (add1 x))
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

(define (sequence-map* f . seqs)
  (struct mapper (seqs)
    #:property prop:sequence
    (λ(m)
      (define (->term m)
        (apply f (map car (map car (mapper-seqs m)))))
      (define (->next m)
        (let ((gens (map cdr (mapper-seqs m))))
          (mapper (map (λ(g) (values->pair (g))) (map cdr (mapper-seqs m))))))
      (define (next? m)
        (andmap car (mapper-seqs m)))
      (make-do-sequence
       (λ()
         (values ->term
                 ->next
                 m
                 next?
                 #f #f)))))
  (mapper (map (λ(s) (values->pair (sequence-generate* s))) seqs)))

#|
(define-generics passable
  (current-term passable)
  (next-passable passable)
  (continue? passable)
  (initializer passable info)
  (pull-info passable)
  #:fallbacks
  ((define (initializer me passable) me)))

(define (lift-passable seq-proc)
  (struct lifted (term generator last-good-term)
    #:methods gen:passable
    ((define (current-term l)
       (let ((t (lifted-term l)))
         (if t (car t) t)))
     (define (next-passable l)
       (let-values (((t g) ((lifted-generator l))))
         (lifted t g (current-term l))))
     (define (continue? l)
       (lifted-term l))
     (define (initializer me val)
       (let-values (((t g) (sequence-generate* (seq-proc val))))
         (lifted t g val)))
     (define (pull-info passable)
       (lifted-last-good-term passable))))
  (lifted #f #f #f))

(define (pass-sequence first-info . seqs)
  (when (not (andmap passable? seqs))
    (error 'pass-sequence
           "Expected a passable sequences: ~a" seqs))
  (define (->term pos)
    (current-term (car pos)))
  (define (->next pos)
    (let ((head (next-passable (car pos)))
          (tail (cdr pos)))
      (if (continue? head)
          (cons head tail)
          (if (null? tail)
              (cons head tail)
              (let loop ((head (initializer (car tail) (pull-info head)))
                         (tail (cdr tail)))
                (cond ((null? tail)
                       (cons head tail))
                      ((continue? head)
                       (cons head tail))
                      (else
                       (loop (initializer (car tail) (pull-info head))
                             (cdr tail)))))))))
  (define (include-term? pos)
    (continue? (car pos)))
  (make-do-sequence
   (λ()
     (values ->term
             ->next
             (cons (initializer (car seqs) first-info)
                   (cdr seqs))
             include-term?
             #f #f)))) ;|#