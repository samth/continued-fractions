#lang racket/base
(require "general-continued-fractions.rkt"
         "simple-matrix.rkt"
         "sequence-utils.rkt"
         )

; EVERY public function must return a simple continued fraction
;   as a SEQUENCE of TERMS
(provide consume-limit
         general->simple general->simple/base
         rational->cf
         pi-cf
         )

(define-syntax-rule (value-check val (tests ...) fail)
  (if (andmap (λ(?) (? val)) (list tests ...))
      val
      fail))

(define (consume-limit-guard n)
  (value-check n (number? exact? integer? positive?)
               (error 'consume-limit
                      "Expected an exact positive integer: ~a" n)))

(define consume-limit (make-parameter 500 consume-limit-guard))

(define (rational->cf rat)
  (value-check rat (exact? rational?)
               (error 'rational->cf
                      "Expected an exact rational: ~a" rat))
  (struct rat-cf (term num den)
    #:property prop:sequence
    (λ(p)
      (define (current rcf) (rat-cf-term rcf))
      (define (next rcf)
        (if (zero? (rat-cf-den rcf))
            (rat-cf #f #f #f)
            (let-values (((q r) 
                          (quotient/remainder (rat-cf-num rcf) (rat-cf-den rcf))))
              (rat-cf q (rat-cf-den rcf) r))))
      (make-do-sequence
       (λ() (values current
                    next
                    (next p)
                    current
                    #f #f)))))
  (let ((num (numerator rat))
        (den (denominator rat)))
    (rat-cf #f num den)))

(define (rational->matrix num den)
  (for/fold ((accum '((1 0) (0 1))))
    ((m (rational->cf (/ num den))))
    (matrix* accum `((,m 1) (1 0)))))

(define (nothing)
  (values #f nothing))

#|
Standard emitter transform: ((0 1) (1 (- t)))
Base B ((B (- (* B t))) (0 1))
|#

(define standard-transformer
  (λ(t) `((0 1) (1 ,(- t)))))

(define (base-transformer base)
  (λ(t) `((,base ,(- (* base t)))(0 1))))

(define (general->simple state-matrix generated-sequence)
  (make-consumer-emitter standard-transformer 
                         state-matrix
                         generated-sequence))

(define (general->simple/base base state-matrix generated-sequence)
  (make-consumer-emitter (base-transformer base)
                         state-matrix generated-sequence))

(define (make-consumer-emitter transform-matrix state-matrix generated-sequence)  
  (struct consume-emitter (state-matrix generated-sequence)
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
        (apply quotient (map car (consume-emitter-state-matrix sce))))
      (define (yield? state)
        (let ((q1 (map car state))
              (q2 (map cadr state)))
          (if (or (zero? (cadr q1))
                  (zero? (cadr q2)))
              #f
              (= (apply quotient q1) 
                 (apply quotient q2)))))
      (define (consume-loop sce)
        (let loop ((state (consume-emitter-state-matrix sce))
                   (gen-seq (consume-emitter-generated-sequence sce))
                   (count (consume-limit)))
          (let ((next-term (gen->term gen-seq)))
            (if (or (zero? count) (not next-term)) ;we're finishing the rational
                (consume-emitter (apply rational->matrix (map car state)) (values->pair (nothing)))
                (let ((new-state (matrix* state next-term)))
                  (if (yield? new-state)
                      (consume-emitter new-state (gen->next-gen gen-seq))
                      (loop new-state (gen->next-gen gen-seq) (sub1 count))))))))
      (define (next-term sce)
        (let* ((t (current-term sce))
               (new-state (matrix* (transform-matrix t) (consume-emitter-state-matrix sce))))
          (consume-loop (consume-emitter new-state (consume-emitter-generated-sequence sce)))))
      (define (continue? sce)
        (or (gen->term (consume-emitter-generated-sequence sce))
            (not (equal? '((1 0) (0 1)) (consume-emitter-state-matrix sce)))))
      (make-do-sequence
       (λ()
         (values current-term
                 next-term
                 (if (yield? (consume-emitter-state-matrix p)) p (consume-loop p))
                 continue?
                 #f #f)))))
  (consume-emitter state-matrix generated-sequence))

(struct simple-consumer-emitter (state-matrix transform-matrix sequence)
  #:methods gen:passable
  ((define (gen->term gen-seq)
     (let ((r (car gen-seq)))
       (if r (car r) r)))
   (define (gen->next-gen gen-seq)
       (if (gen->term gen-seq)
           (values->pair ((cdr gen-seq)))
           gen-seq))
   (define (yield? state)
     (let ((q1 (map car state))
           (q2 (map cadr state)))
       (if (or (zero? (cadr q1))
               (zero? (cadr q2)))
           #f
           (= (apply quotient q1) 
              (apply quotient q2)))))
   (define (consume-loop sce)
       (let loop ((state (simple-consumer-emitter-state-matrix sce))
                  (gen-seq (simple-consumer-emitter-sequence sce))
                  (count (consume-limit)))
         (let ((next-term (gen->term gen-seq)))
           (if (or (zero? count) (not next-term)) ;we're finishing the rational
               (simple-consumer-emitter (apply rational->matrix (map car state)) (values->pair (nothing)))
               (let ((new-state (matrix* state next-term)))
                 (if (yield? new-state)
                     (simple-consumer-emitter new-state
                                              (simple-consumer-emitter-transform-matrix sce)
                                              (gen->next-gen gen-seq))
                     (loop new-state (gen->next-gen gen-seq) (sub1 count))))))))
   (define (current-term passable)
     (apply quotient (map cadr (simple-consumer-emitter-state-matrix passable))))
   (define (next-passable passable)
     (let* ((t (current-term passable))
            (new-state (matrix* ((simple-consumer-emitter-transform-matrix passable) t) (simple-consumer-emitter-state-matrix passable))))
       (consume-loop (simple-consumer-emitter new-state 
                                              (simple-consumer-emitter-transform-matrix passable)
                                              (simple-consumer-emitter-sequence passable)))))
   (define (continue? passable)
     (or (gen->term (simple-consumer-emitter-sequence passable))
         (not (equal? '((1 0) (0 1)) (simple-consumer-emitter-state-matrix passable)))))
   (define (initializer passable info)
     (define s
       (simple-consumer-emitter info 
                                (simple-consumer-emitter-transform-matrix passable)
                                (values->pair (sequence-generate* (simple-consumer-emitter-sequence passable)))))
       (if (yield? info)
           s
           (consume-loop s)))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;tests
(define pi-tail-cf
  (make-general-cf (enumerate-naturals 0 (λ(x) (add1 (* 2 x)))) (enumerate-naturals 1 (λ(x) (* x x)))
                   (endless-values 1) (endless-values 0)))
(define pi-cf (simple-consumer-emitter 'nothing
                                       (base-transformer 10)
                                       pi-tail-cf))
#|
(for ((t (pass-sequence '((0 4) (1 0)) pi-cf))
        (i (in-range 20)))
    (printf "~a " t))
|#
