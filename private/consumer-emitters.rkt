#lang racket/base
(require "general-continued-fractions.rkt"
         "simple-matrix.rkt"
         "sequence-utils.rkt"
         )

(provide consume-limit
         general->simple
         )

(define-syntax-rule (value-check val (tests ...) fail)
  (if (andmap (λ(?) (? val)) (list tests ...))
      val
      fail))

(define (consume-limit-guard n)
  (value-check n (number? exact? integer? positive?)
         (error 'consume-limit
                "Expected an exact positive integer: ~a" n)))

(define consume-limit (make-parameter 50 consume-limit-guard))

(define (rational->matrix num den)
  (define (r->s n d)
    (let-values (((q r) (quotient/remainder n d)))
      (if (zero? r)
          (list q)
          (cons q (r->s d r)))))
  (define id '((1 0) (0 1)))
  (if (zero? den)
      id
      (foldl (λ(t m) (matrix* m `((,t 1) (1 0)))) id
             (r->s num den))))

(define (nothing)
  (values #f nothing))

(struct general->simple (state-matrix generated-sequence)
  #:property prop:sequence
  (λ(p)
    (define (gen->term gen-seq)
      (let ((r (car gen-seq)))
        (if r (car r) r)))
    (define (gen->next-gen gen-seq)
      (if (gen->term gen-seq)
          (values->pair ((cdr gen-seq)))
          gen-seq))
    (define (current-term sce) ;assume state-matrix is in a valid state
      (apply quotient (map car (general->simple-state-matrix sce))))
    (define (yield? state)
      (let ((q1 (map car state))
            (q2 (map cadr state)))
        (if (or (zero? (cadr q1))
                (zero? (cadr q2)))
            #f
            (= (apply quotient q1) 
               (apply quotient q2)))))
    (define (consume-loop sce)
      (let loop ((state (general->simple-state-matrix sce))
                 (gen-seq (general->simple-generated-sequence sce))
                 (count (consume-limit)))
        (let ((next-term (gen->term gen-seq)))
          (if (or (zero? count) (not next-term)) ;we're finishing the rational
              (general->simple (apply rational->matrix (map car state)) (values->pair (nothing)))
              (let ((new-state (matrix* state next-term)))
                (if (yield? new-state)
                    (general->simple new-state (gen->next-gen gen-seq))
                    (loop new-state (gen->next-gen gen-seq) (sub1 count))))))))
    (define (next-term sce)
      (let* ((t (current-term sce))
             (new-state (matrix* `((0 1) (1 ,(- t))) (general->simple-state-matrix sce))))
        (consume-loop (general->simple new-state (general->simple-generated-sequence sce)))))
    (define (continue? sce)
      (or (gen->term (general->simple-generated-sequence sce))
          (not (equal? '((1 0) (0 1)) (general->simple-state-matrix sce)))))
    (make-do-sequence
     (λ()
       (values current-term
               next-term
               (if (yield? (general->simple-state-matrix p)) p (consume-loop p))
               continue?
               #f #f)))))

