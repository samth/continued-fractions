#lang racket/base
(require (for-syntax racket/base)
         racket/generic racket/sequence
         "general-continued-fractions.rkt"
         "simple-matrix.rkt"
         "sequence-utils.rkt"
         )

; EVERY public function must return a simple continued fraction
;   as a SEQUENCE of TERMS
(provide consume-limit precision
         standard-transformer base-transformer
         general->simple-cf
         general-arithmetic->cf
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

(define (precision-guard n)
  (value-check n (number? positive?)
               (error 'precision
                      "Expected a positive number: ~a" n)))
(define precision (make-parameter (expt 2 30) precision-guard))

(define (rational->matrix num den)
  (for/fold ((accum '((1 0) (0 1))))
            ((m (rational->cf (/ num den))))
    (matrix* accum `((,m 1) (1 0)))))


; We have a generic interface that continued fractions must provide.
; This way, underneath the hood, one continued fraction expression
; can become another. For instance when a precision limit is reached,
; the continued fraction should die as a rational instead of an
; irrational. Or during arithmetic one sequence may die, then the
; form can be simplified from the bihomographic to the standard
; case. Racket sequences cannot "append themselves" without
; convoluted constructs so some sort of parent sequence is needed.
(define-generics consumer-emitter
  (->term consumer-emitter)
  (emit-transform consumer-emitter)
  (emit-ok? consumer-emitter)
  (->state consumer-emitter)
  (push-term consumer-emitter)
  (emit consumer-emitter)
  (consume consumer-emitter)
  (consume-loop consumer-emitter)
  (finish consumer-emitter))

(struct continued-fraction (consumer-emitter)
  #:property prop:sequence
  (λ(scf)
    (when (not (consumer-emitter? (continued-fraction-consumer-emitter scf)))
      (error 'continued-fraction "Expected a consumer-emitter: ~a"
             (continued-fraction-consumer-emitter scf)))
    (make-do-sequence
     (λ()
       (values (λ(scf) (->term (continued-fraction-consumer-emitter scf)))
               (λ(scf) (continued-fraction
                        (emit (continued-fraction-consumer-emitter scf))))
               scf
               (λ(scf) (emit-ok? (continued-fraction-consumer-emitter scf)))
               #f #f)))))

(define standard-transformer
  (λ(t) `((0 1) (1 ,(- t)))))

(define (base-transformer base)
  (λ(t) `((,base ,(- (* base t)))(0 1))))

(define (general->simple-cf general
                            #:transformer (T standard-transformer)
                            #:force (times 1))
  (value-check times (number? exact? positive?)
               (error 'general->simple-cf "Expected a positive exact integer: ~a" times))
  (value-check general (sequence?)
               (error 'general->simple-cf "Expected a sequence: ~a" general))
  (let ((scf (general->simple-ce general #:transformer T)))
    (continued-fraction
     (consume-loop
      (force-consume scf times)))))

(define (force-consume ce times) ; some simple continued fractions are only well-behaved
  ; after consuming some number of general terms
  (when (not (consumer-emitter? ce))
    (error 'force-consume "Expected a consumer-emitter: ~a" ce))
  (for/last ((i (in-range times)))
    (consume ce)))

(define (standard-consume-loop consumer-emitter)
  (define (cl ce tries)
    (cond ((zero? tries) (finish ce))
          ((emit-ok? ce) (push-term ce))
          (else
           (cl (consume ce) (sub1 tries)))))
  (cl consumer-emitter (consume-limit)))

(define (standard-emit-ok? ce)
  (local-require (only-in racket/math sgn))
  (define state (->state ce))
  (define denominators (cadr state))
  (define (interval-check state)
    (local-require racket/match)
    (match state
      ((list (list a b) (list c d))
       (= (quotient (+ a b) (+ c d)) ; x->1
          (quotient a c))) ; x->infty
      ((list (list a b c d) (list e f g h))
       (= (quotient (+ a b) (+ e f)) ; x->infty y->1
                (quotient (+ a c) (+ e g)) ; x->1 y->infty
                (quotient (+ a b c) (+ e f g)) ; x->infty y->infty
                (quotient (+ a b c d) (+ e f g h)) ; x->1 y->1
                ))))
  (and state ;if this is #f then we've hit precision
       (andmap (λ(v) (not (zero? v))) denominators) ; no zeros in the denominator
       ;(apply = (map sgn denominators)) ; we have an outside situation
       (interval-check state)))

(define (standard-finish consumer-emitter)
  ; TODO: finish rational in case of general sequence termination
  (void))

(define 2x2-identity '((1 0) (0 1)))

(define (general->simple-ce general-cf #:transformer (T standard-transformer) #:start-state (state 2x2-identity))
  (struct simple (term state accum-denoms next-term generator)
    #:methods gen:consumer-emitter
    [(define (->term ce)
       (simple-term ce))
     (define (->state ce)
       (simple-state ce))
     (define (emit-ok? ce)
       (standard-emit-ok? ce))
     (define (push-term ce)
       ; take the term from the state matrix
       ; if we've reached precision, then kill the state matrix
       (let ((term* (apply quotient (map car (simple-state ce))))
             (state (if (< (abs (apply * (simple-accum-denoms ce))) (precision))
                        (simple-state ce) #f)))
         (simple term*
                 state
                 (simple-accum-denoms ce)
                 (simple-next-term ce)
                 (simple-generator ce))))
     (define (emit ce)
       ; use the term updated from push-term, this means:
       ;   * update the accumulator for precision-checking
       ;   * apply the output transformer to the state
       (define (denominators state) (cadr state))
       (define (update-denoms denoms term)
         (let ((c (car denoms))
               (d (cadr denoms)))
           (list (+ d (* c term)) c)))
       (let* ((term (simple-term ce))
              (state* (matrix* (T term) (simple-state ce)))
              (accum-state* (update-denoms (simple-accum-denoms ce) term))
              (next-term (simple-next-term ce))
              (generator (simple-generator ce)))
         (consume-loop (simple term state* accum-state* next-term generator))))
     (define (consume ce)
       ; take the term from the generated sequence and update the matrix
       ; then move the generator one term
       (define term (simple-term ce))
       (define state* (matrix* (simple-state ce)
                               (simple-next-term ce)))
       (define accum-state (simple-accum-denoms ce))
       (define-values (next-term* generator*)
         ((simple-generator ce)))
       (simple term state* accum-state (car next-term*) generator*))
     (define (consume-loop ce)
       (standard-consume-loop ce))
     (define (finish ce)
       (standard-finish ce))])
  (define-values (next gen) (sequence-generate* general-cf))
  (simple #t state '(1 0) (car next) gen))

(module+ test
  (require rackunit racket/sequence
           "sequence-utils.rkt")
  (define (check-cf-equal? cf reference)
    (define (pull n cf)
      (parameterize ((precision +inf.0))
        (for/list ((t cf)
                   (i (in-range n)))
          t)))
    (check-equal? (pull (length reference) cf) reference))
  
  (define transformer (base-transformer 10))
  (define pi-general (make-general-cf (sequence-append (list 0) (enumerate-naturals 0 (λ(x) (add1 (* 2 x)))))
                                      (sequence-append (list 4) (enumerate-naturals 1 (λ(x) (* x x))))
                                      (endless-values 1) (endless-values 0)))
  (define pi-cf (general->simple-cf pi-general #:transformer transformer))
  (define pi-reference '(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 3 3 8 3
                           2 7 9 5 0 2 8 8 4 1 9 7 1 6 9 3 9 9 3 7 5 1 0 5 8 2 0
                           9 7 4 9 4 4 5 9 2 3 0 7 8 1 6 4 0 6 2 8 6 2 0 8 9 9 8
                           6 2 8 0 3 4 8 2 5 3 4 2 1 1 7 0 6 7 9 8 2 1 4 8 0 8 6
                           5 1 3 2 8 2 3 0 6 6 4 7 0 9 3 8 4 4 6))
  (check-cf-equal? pi-cf
                   pi-reference)
  (define sqrt2-general
    (make-general-cf (sequence-append (list 1) (endless-values 5))
                     (sequence-append (list 1) (endless-values 2))
                     (sequence-append (list 1) (endless-values 2))
                     (sequence-append (list 0) (endless-values 1))))
  (define sqrt2-cf (general->simple-cf sqrt2-general #:transformer transformer))
  (define sqrt2-reference '(1 4 1 4 2 1 3 5 6 2 3 7 3 0 9 5 0 4 8 8 0 1 6 8 8 7
                              2 4 2 0 9 6 9 8 0 7 8 5 6 9 6 7 1 8 7 5 3 7 6 9 4
                              8 0 7 3 1 7 6 6 7 9 7 3 7 9 9 0 7 3 2 4 7 8 4 6 2
                              1 0 7 0 3 8 8 5 0 3 8 7 5 3 4 3 2 7 6 4 1 5 7 2 7
                              3 5 0 1 3 8 4 6 2 3 0 9 1 2 2 9 7 0 2 4 9 2 4 8 3
                              6 0))
  (check-cf-equal? sqrt2-cf
                   sqrt2-reference)
  )

(define (general-arithmetic->cf g1 g2 #:transformer (T standard-transformer) #:start-state state)
  (continued-fraction
   (consume-loop
    (general-arithmetic-base g1 g2 #:transformer T #:start-state state))))

(define (general-arithmetic-base gcf-p gcf-q #:transformer (T standard-transformer) #:start-state state)
  ; state here is not optional since it decides the arithmetic
  (struct consumer-emitter-2 (out-term state accum-denoms p-next p-generator q-next q-generator)
    #:methods gen:consumer-emitter
    [(define (->term ce)
       (consumer-emitter-2-out-term ce))
     (define (->state ce)
       (consumer-emitter-2-state ce))
     (define (emit-ok? ce)
       (standard-emit-ok? ce))
     (define (push-term ce)
       ; take the term from the state matrix
       ; if we've reached precision, then kill the state matrix
       (define state (consumer-emitter-2-state ce))
       (let ((term* (apply quotient (map car state)))
             (state* (if (< (abs (apply * (consumer-emitter-2-accum-denoms ce))) (precision))
                         state #f)))
         (consumer-emitter-2 term*
                             state*
                             (consumer-emitter-2-accum-denoms ce)
                             (consumer-emitter-2-p-next ce)
                             (consumer-emitter-2-p-generator ce)
                             (consumer-emitter-2-q-next ce)
                             (consumer-emitter-2-q-generator ce))))
     (define (emit ce)
       ; use the term updated from push-term, this means:
       ;   * update the accumulator for precision-checking
       ;   * apply the output transformer to the state
       (define state (consumer-emitter-2-state ce))
       (define denominators (cadr state))
       (define (update-denoms denoms term)
         (let ((c (car denoms))
               (d (cadr denoms)))
           (list (+ d (* c term)) c)))
       (let* ((term (->term ce))
              (state* (matrix* (T term) state))
              (accum-state* (update-denoms denominators term))
              (p (consumer-emitter-2-p-next ce))
              (p-g (consumer-emitter-2-p-generator ce))
              (q (consumer-emitter-2-q-next ce))
              (q-g (consumer-emitter-2-q-generator ce)))
         (consume-loop (consumer-emitter-2 term state* accum-state* p p-g q q-g))))
     (define (consume ce)
       ; take the term from the generated sequence and update the matrix
       ; then move the generator one term
       ; right now we stupidly take one term from each continued fraction p, q
       ; in principle we could optimize it to see which term would give us more info
     (define term (->term ce))
       (define-syntax submatrix
         (syntax-rules (p q)
         ((_ p state)
          (let-values (((a b c d) (apply values (car state)))
                       ((e f g h) (apply values (cadr state))))
            (values (list (list a c) (list e g))
                    (list (list b d) (list f h)))))
         ((_ q state)
          (let-values (((a b c d) (apply values (car state)))
                       ((e f g h) (apply values (cadr state))))
            (values (list (list a b) (list e f))
                    (list (list c d) (list g h)))))))
     (define-syntax assemble
       (syntax-rules (p q)
         ((_ p l r)
          (let-values (((a c) (apply values (car l)))
                       ((e g) (apply values (cadr l)))
                       ((b d) (apply values (car r)))
                       ((f h) (apply values (cadr r))))
            (list (list a b c d) (list e f g h))))
         ((_ q l r)
          (let-values (((a b) (apply values (car l)))
                       ((e f) (apply values (cadr l)))
                       ((c d) (apply values (car r)))
                       ((g h) (apply values (cadr r))))
            (list (list a b c d) (list e f g h))))))
     (define (update-p state) ; state->state
       (define-values (p-l p-r) (submatrix p state))
       (define P (consumer-emitter-2-p-next ce))
       (assemble p
                 (matrix* p-l P)
                 (matrix* p-r P)))
     (define (update-q state)
       (define-values (q-l q-r) (submatrix q state))
       (define Q (consumer-emitter-2-q-next ce))
       (assemble q
                 (matrix* q-l Q)
                 (matrix* q-r Q)))
     (let* ((state (consumer-emitter-2-state ce))
            (state+ (update-p state))
            (state* (update-q state+)))
       (let ((p-g (consumer-emitter-2-p-generator ce))
             (q-g (consumer-emitter-2-q-generator ce)))
         (let-values (((p* p-g*) (p-g))
                      ((q* q-g*) (q-g)))
           (consumer-emitter-2 term
                               state*
                               (consumer-emitter-2-accum-denoms ce)
                               (car p*)
                               p-g*
                               (car q*)
                               q-g*)))))
   (define (consume-loop ce)
     (standard-consume-loop ce))
   (define (finish ce)
     (standard-finish ce))])
  (let-values (((p p-g) (sequence-generate* gcf-p))
               ((q q-g) (sequence-generate* gcf-q)))
    (consumer-emitter-2 #t state '(1 0) (car p) p-g (car q) q-g)))

(module+ test
  (define root2+pi-reference '(4 5 5 5 8 0 6 2 1 5 9 6 2 8 8 8 2 8 7 2 6 4 3 3
                                 2 1 0 7 4 8 9 2 0 0 9 6 2 7 6 6 8 4 1 2 7 4 7
                                 5 2 0 5 3 8 9 4 1 5 1 6 2 4 3 3 0 2 9 8 5 4 8
                                 8 8 4 7 4 8 3 1 6 0 3 7 4 7 8 4 2 2 3 5 9 6 6
                                 9 7 5 8 6 4 0 7 1 7 1 6 1 9 3 2 7 4 4 1 9 4 6
                                 0 3 6 7 2 0 1 8 6 8 0 6))
  (define root2+pi-cf
    (general-arithmetic->cf sqrt2-general pi-general #:transformer transformer #:start-state '((0 1 1 0) (0 0 0 1))))
  (check-cf-equal? root2+pi-cf
                   root2+pi-reference)
  (define root2-pi-reference '(-1 -7 -2 -7 -3 -7 -9 0 -9 -1 -2 -1 -6 -6 -9 -8
                                  -1 -8 -9 -6 -6 0 -9 -5 -4 -6 -5 -9 0 -6 -9
                                  -8 0 -4 -8 0 -5 -6 -2 -7 -4 -9 -7 -5 -2 -3
                                  -9 -9 -8 -1 -5 -7 -7 -4 -7 -7 -9 -8 -2 -6
                                  -4 -8 -5 -4 -3 -1 -7 0 -8 -3 -9 -2 -7 -8 -2
                                  -4 -1 0 -1 -9 -5 -9 -7 -7 -7 -6 -4 -7 -2 -9
                                  -1 0 -1 -4 -4 -7 -5 -4 -9 -5 -2 -4 -7 -1 -3
                                  -4 -2 -4 0 -2 -8 -2 -3 -7 0 0 0 -9 -6 -2 -2
                                  -1 -6 -9 0 0 -8 -5 -5 -3 -6 -9 -9 -8 -4 -5
                                  0 -1 -9 0 -8 -1 -2 -3 -7 -9 -1 -1 0 -2 -8
                                  -5 -4 -5 -2 -8 -6 0 -3 -7 0 -6 -1 -4 -3 -6
                                  -7 -7 -4 -4 -3 -2 -9 -2 -8 -3 -4 -9 -7 -5
                                  -9 -6 -9 -4 -1 -2 -1 -7 -9 -6 -1 -7 -2 -8
                                  -6 -9 -8 -1 0 -4 -9 -4 -3 -1 -9 -2 -5 -1 0
                                  0 -4 0 -5 -9 -9 -6 -3 -1 -7 -1 -5 -9 -3 -8
                                  -7 -8 -7 -8 -6 -2 -1 -9 0 -5 -8 -2 -6 -5
                                  -7 -4 -6 -6 -3 0 -3 -1 -2 -7 -1 0 -5 -3 -6
                                  0 -9 -6))
  (define root2-pi-cf
    (general-arithmetic->cf sqrt2-general pi-general #:transformer transformer #:start-state '((0 1 -1 0) (0 0 0 1))))
  (check-cf-equal? root2-pi-cf
                   root2-pi-reference)
  )
