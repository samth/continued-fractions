#lang racket/base
(require (for-syntax racket/base syntax/parse racket/syntax)
         racket/generic racket/sequence syntax/parse/define
         racket/match racket/list racket/local
         ;"general-continued-fractions.rkt"
         "simple-matrix.rkt"
         "sequence-utils.rkt"
         )

; EVERY public function must return a simple continued fraction
;   as a SEQUENCE of TERMS
(provide consume-limit precision
         standard-transformer base-transformer
         rat simple-arithmetic cfce1
         ;(struct-out continued-fraction)
         )

(define (quotient*/remainder a b)
  (local-require racket/math)
  (define a/b*
    (let ((b* (conjugate b)))
      (/ (* a b*) (* b b*))))
  (let ((r (real-part a/b*))
        (i (imag-part a/b*)))
    (let ((qr (round r))
          (qi (round i)))
      (let ((q (make-rectangular qr qi)))
        (let ((r (- a (* b q))))
          (values q r))))))

(define (quotient* a b)
  (let-values (((q r) (quotient*/remainder a b)))
    q))

(define (sign x)
  (if (negative? x)
      -1
      1))

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
  (->next consumer-emitter)
  (emit-ok? consumer-emitter)
  (init consumer-emitter)
  (emit consumer-emitter)
  (consume consumer-emitter limit)
  )


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
                        (->next (continued-fraction-consumer-emitter scf))))
               scf
               (λ(scf) (emit-ok? (continued-fraction-consumer-emitter scf)))
               #f #f))))) ;|#

(define (standard-transformer t)
  (list (list 0 1) (list 1 (- t))))
  ;(λ(t) `((0 1) (1 ,(- t)))))

(define (base-transformer base)
  (λ(t) (list (list base (- (* base t)))
              (list 0 1))))
  ;(λ(t) `((,base ,(- (* base t)))(0 1))))


(define-simple-macro (define-struct-fields struct:id (field:id ...) v:expr)
  #:with s (generate-temporary #'struct)
  #:with [struct-field ...]
  (for/list ([field (in-list (syntax->list #'(field ...)))])
    (format-id #'struct "~a-~a" #'struct field))
  (begin
    (define s v)
    (define field (struct-field s))
    ...))

(define-simple-macro (ce-struct struct-name (accessors ...) #:methods gen [body ...] #:property prop)
  #:with sv (datum->syntax #'struct-name 'struct-values)
  #:with ooo (quote-syntax ...)
  #:with exp* #'(λ(ce)
                  (make-do-sequence
                   (λ()
                     (values ->term
                             ->next
                             (init ce)
                             emit-ok?
                             #f #f))))
  (struct struct-name (accessors ...)
    #:transparent
    #:methods gen
    [(define-syntax-rule (sv (fld ooo) v)
       (define-struct-fields struct-name (fld ooo) v))
     body ...]
    #:property prop
    exp*))

; when creating a rat, pass the rational in as a term
; init will handle the conversion to its usual structure
; the rat ce just holds a list of expanded terms for the rational
; it isn't really like the other consumer-emitters
(ce-struct rat (term)
  #:methods gen:consumer-emitter
  [(define (->term ce)
     (car (rat-term ce)))
   (define (->next ce)
     (if (null? (cdr (rat-term ce)))
         (rat #f)
         (rat (cdr (rat-term ce)))))
   (define (init ce)
     ; first emission may be negative so it is a special case
     ; -a -b -c -d ... = -a-1 1 b-1 c d ...
     ; if we are handed a sequence as in (finish ce)
     ; then the terms will already be positive as assured by
     ; the algorithm
     ; this is really messy because subtraction of the second term
     ; might yield a 0 and then a 1 0 c ... = a (+ 1 c) ...
     (define r (rat-term ce))
     (define cf
       (let loop ((n (numerator r))
                  (d (denominator r)))
         (let-values (((q r) (quotient/remainder n d)))
           (if (zero? r)
               (list q)
               (cons q (loop d r))))))
     (define (adjust cf)
       (define (fix-last-1 t)
         (match t
           ((list a 1 1)
            (list a 2))
           (_ t)))
       (define (usual-fix cf)
         (let ((h (take cf 2))
               (t (drop cf 2)))
           (let ((a (car h))
                 (b (abs (cadr h))))
             (if (= 1 b)
                 (let ((c (car t)))
                   (append (list (sub1 a) (add1 (abs c)))
                           (map abs (cdr t))))
                 (append (list (sub1 a) 1 (sub1 (abs b)))
                         (map abs t))))))
       (define (dispatch cf)
         (match cf
           ((list (? negative? a)) (list a))
           ((list (? negative? a) (? negative? b))
            (fix-last-1 (list (sub1 a) 1 (sub1 (abs b)))))
           ((list (? negative? a) (? negative?) ...)
            (usual-fix cf))
           (_ cf)))
       (if (zero? (car cf))
           (cons 0 (dispatch (cdr cf)))
           (dispatch cf)))
     (rat (adjust cf)))
   (define (emit-ok? ce)
     (rat-term ce))]
  #:property prop:sequence)
  
(define (rational->cf rational)
  (init (rat rational)))

(module+ test
  (require rackunit)
  (define (pull cf . terms)
    (if (null? terms)
        (let ((res (time (for/list ((t cf)) t))))
          (displayln res) res)
        (let ((res (time (for/list ((t cf) (i (in-range (car terms)))) t))))
          (displayln res) res)))
  (check-equal? (pull (rat 123/456))
                '(0 3 1 2 2 2 2)
                "Rational check: continued fraction for 123/456")
  ; (0 -3 -1 -2 -2 -2 -2) ->
  ; (0 -4 1 0 2 2 2 2) ->
  ; (0 -4 3 2 2 2)
  (check-equal? (pull (rat -123/456))
                '(0 -4 3 2 2 2)
                "Negative rational test of -123/456")
  (check-equal? (pull (rat -2/3))
                '(0 -2 2)
                "Negative rational test of -2/3"))

(define (kar it)
  (if it (car it) it))

(ce-struct simple-arithmetic (term state inner-term generator)
  #:methods gen:consumer-emitter
  [(define (->term ce)
     (simple-arithmetic-term ce))
   (define (->next ce)
     ; take the term already pulled by the sequence out of the state matrix
     ; update the generator
     ; start looping to generate the next term
     (struct-values (term state inner-term generator) ce)
     (if term
         (let ((state* (matrix* (standard-transformer term)
                                state)))
           (let-values (((it gen) (generator)))
             (consume (simple-arithmetic term state* (kar it) gen) (consume-limit))))
         (consume ce (consume-limit))))
   (define (->finish state)
     (displayln "consume limit reached" (current-error-port))
     (rational->cf (apply / (map car state))))
   (define (init ce)
     ; here we have a special cycle like in (rat ...) to ensure only the first
     ; term we emit is negative
     ; but, sometimes the first term is 0 so there is some jiggery pokery
     ; to peek ahead and see if the next term is negative, and to fix it
     ; TODO: actually do this
     (consume ce (consume-limit)))
     #|
     (define ce* (consume ce (consume-limit)))
     (struct-values (term state inner-term generator) ce*)
     (if (negative? term)
         (simple-arithmetic (sub1 term) state inner-term generator)
         (if (zero? term)
             (let ((ce** (->next ce*)))
               (if (negative? (simple-arithmetic-term ce**))
                   (let-values (((it gen) (sequence-generate* ce**)))
                     (simple-arithmetic term state (kar it) gen))
                   ce*))
             ce*))) ;|#
   (define (consume ce limit)
     ; if the generator died, then finish the rational
     ; if we reached our limit for consume attempts, finish the rational
     ; otherwise
     ; take the generated term and apply it to the state matrix
     ; see if we can emit a term
     ; if so, put the term in the field
     ; otherwise, consume again
     (define (lift term)
       (if (number? term)
           (list (list term 1) (list 1 0))
           term))
     (struct-values (term state inner-term generator) ce)
     (if (or (not inner-term)
             (zero? limit))
         (->finish state)
         (let* ((state* (matrix* state (lift inner-term)))
                (maybe-t (emit-check term state*)))
           (if maybe-t
               (simple-arithmetic maybe-t state* inner-term generator)
               (let-values (((it gen) (generator)))
                 (consume (simple-arithmetic term state* (kar it) gen) (sub1 limit)))))))
   (define (emit-check current-term state)
     (let-values (((a b) (apply values (car state)))
                  ((c d) (apply values (cadr state))))
       (let ((a+b (+ a b))
             (c+d (+ c d)))
         (and (not (zero? c))
              (not (zero? c+d))
              (= (sign c) (sign c+d))
              (let ((q1 (quotient a c))
                    (q2 (quotient a+b c+d)))
                (and (= q1 q2)
                     (if (negative? q1)
                         (if (or (not current-term)
                                 (zero? current-term)) ; we may be in a 0 -a -b -c ...
                             (sub1 q1)
                             #f)
                         q1)))))
       #;(and (not (zero? c))
            (not (zero? d))
            (let ((q1 (quotient a c))
                  (q2 (quotient b d)))
              (and (= q1 q2)
                   (if (negative? q1)
                       (if (and current-term (zero? current-term))
                           (sub1 q1)
                           #f)
                       q1))))
       ))
   (define (emit-ok? ce)
     (simple-arithmetic-term ce))]
  #:property prop:sequence)

(define (cfce1 cf state)
  (let-values (((it gen) (sequence-generate* cf)))
    (simple-arithmetic #f state (kar it) gen)))

(module+ test
  (check-equal? (pull (cfce1 (rat 123/456) '((1 5) (0 1))))
                (pull (rat 801/152))
                "(+ 5 123/456) = 801/152")
  (let ((sqrt-5 (sequence-append (list 2) (endless-values 4))))
    (check-equal? (pull (cfce1 sqrt-5 '((1 1) (0 2))) 128)
                  (build-list 128 (λ(x) 1))
                  "(/ (add1 (sqrt 5)) 2) = phi"))
  (let ((sqrt-5 (sequence-append (list 2) (endless-values 4))))
    ;'(0 -1 -11 -1 -2 -2 -2 -1 -12 -1 -2 -2 -2 -1 -12 -1 -2 -2 -2) ->
    ;'(0 -2 1 10 1  2  2  2  1  12  1  2  2  2  1  12  1  2  2  2)
    (check-equal?  (pull (cfce1 sqrt-5 '((1 -5) (0 3))) 20)
                   '(0 -2 1 10 1  2  2  2  1  12  1  2  2  2  1  12  1  2  2  2))))

