#lang racket/base
(require (for-syntax racket/base racket/syntax)
         racket/generic racket/sequence syntax/parse/define
         racket/match racket/list
         "simple-matrix.rkt"
         "sequence-utils.rkt"
         )

; EVERY public function must return a simple continued fraction
;   as a SEQUENCE of TERMS
(provide consume-limit precision
         consumer-emitter?
         standard-transformer base-transformer
         cf-terms->rational
         rat rational->cf
         simple-arithmetic cfce1
         simple-arithmetic-2 cfce2
         base-emitter cfbe
         precision-emitter cfpe
         )
#|
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
    q)) ;|#

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
  (force consumer-emitter)
  (emit-ok? consumer-emitter)
  (init consumer-emitter)
  (emit consumer-emitter)
  (consume consumer-emitter limit)
  )

(define (standard-transformer t)
  (list (list 0 1) (list 1 (- t))))

(define (base-transformer base)
  (λ(t) (list (list base (- (* base t)))
              (list 0 1))))

(define (cf-terms->rational lots)
  (when (not (and (list? lots)
                  (andmap (λ(t) (and (number? t) (exact? t)))
                          lots)))
    (error 'cf-terms->rational
           "Expected a list of exact numbers."))
  (define (step t accum)
    (if (zero? accum) t (+ t (/ accum))))
  (foldr step 0 lots))


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
    #:methods gen:custom-write
    [(define (write-proc ce port mode)
       (let* ((t (->term (init ce)))
              (base-rep (format "(~a ...)" (if t t 'infinity)))
              (mod (λ(s) (format "<continued-fraction: ~a>" s))))
          (let ((out (case mode
                       ((#t) (λ(v p) (write (mod v) p)))
                       ((#f) display)
                       (else (λ(v p) (print (mod v) p mode))))))
            (out base-rep port))))]
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
       (if (number? r)
           (let loop ((n (numerator r))
                      (d (denominator r)))
             (let-values (((q r) (quotient/remainder n d)))
               (if (zero? r)
                   (list q)
                   (cons q (loop d r)))))
           r))
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
        (for/list ((t cf)) t)
        (for/list ((t cf) (i (in-range (car terms)))) t)))
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
   (define (->finish ce)
     (struct-values (state) ce)
     (let ((ts (map car state)))
       (if (zero? (cadr ts))
           (simple-arithmetic #f #f #f #f)
           (rational->cf (apply / ts)))))
   (define (init ce)
     (consume ce (consume-limit)))
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
         (begin
           (when (zero? limit)
             (displayln "consume limit reached" (current-error-port)))
           (->finish ce))
         (let* ((state* (matrix* state (lift inner-term)))
                (maybe-t (emit-check (simple-arithmetic term state* inner-term generator))))
           (if maybe-t
               (simple-arithmetic maybe-t state* inner-term generator)
               (let-values (((it gen) (generator)))
                 (consume (simple-arithmetic term state* (kar it) gen) (sub1 limit)))))))
   (define (emit-check ce)
     (struct-values (term state) ce)
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
                         (if (or (not term)
                                 (zero? term)) ; we may be in a 0 -a -b -c ...
                             (sub1 q1)
                             #f)
                         q1)))))))
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
    (check-equal? (pull (cfce1 sqrt-5 '((1 1) (0 2))) 64)
                  (build-list 64 (λ(x) 1))
                  "(/ (add1 (sqrt 5)) 2) = phi"))
  (let ((sqrt-5 (sequence-append (list 2) (endless-values 4))))
    ;'(0 -1 -11 -1 -2 -2 -2 -1 -12 -1 -2 -2 -2 -1 -12 -1 -2 -2 -2) ->
    ;'(0 -2 1 10 1  2  2  2  1  12  1  2  2  2  1  12  1  2  2  2)
    (check-equal?  (pull (cfce1 sqrt-5 '((1 -5) (0 3))) 20)
                   '(0 -2 1 10 1  2  2  2  1  12  1  2  2  2  1  12  1  2  2  2))))

(ce-struct precision-emitter (term state accum inner-term generator)
  #:methods gen:consumer-emitter
  [(define (->term ce)
     (precision-emitter-term ce))
   (define (->next ce)
     ; take the term already pulled by the sequence out of the state matrix
     ; start looping to generate the next term
     (struct-values (term state accum inner-term generator) ce)
     (if term
         (if (not state)
             (precision-emitter #f #f #f #f #f)
             (if (not accum) ; we reached precision
                 (precision-emitter #f #f #f #f #f)
                 (let ((state* (matrix* (standard-transformer term)
                                        state))
                       (accum* (if (< (precision) (abs (apply * accum))) #f accum)))
                   (consume (precision-emitter term state* accum* inner-term generator) (consume-limit)))))
         (consume ce (consume-limit))))
   (define (->finish ce)
     (struct-values (state) ce)
     (if (not state)
         (precision-emitter #f #f #f #f #f)
         (let ((ts (map car state)))
           (if (zero? (cadr ts))
               (precision-emitter #f #f #f #f #f)
               (rational->cf (apply / ts))))))
   (define (init ce)
     (consume ce (consume-limit)))
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
     (define (update-accum a t)
       (define-values (p q) (apply values (car t)))
       (define-values (r s) (apply values (cadr t)))
       (define-values (c d) (apply values a))
       (list (+ (* c p) (* d r)) (+ (* c q) (* d s))))
     (struct-values (term state accum inner-term generator) ce)
     (if (zero? limit)
         (begin
           (displayln "consume limit reached" (current-error-port))
           (->finish ce))
         (let ((maybe-t (emit-check ce)))
           (if maybe-t
               (precision-emitter maybe-t state accum inner-term generator)
               (if (not inner-term)
                   (->finish ce)
                   (let* ((t (lift inner-term))
                          (state* (matrix* state t))
                          (accum* (and accum (update-accum accum t))))
                     (let-values (((it gen) (generator)))
                       (consume (precision-emitter term state* accum* (kar it) gen) (sub1 limit)))))))))
   (define (emit-check ce)
     (struct-values (term state) ce)
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
                         (if (or (not term)
                                 (zero? term)) ; we may be in a 0 -a -b -c ...
                             (sub1 q1)
                             #f)
                         q1)))))))
   (define (emit-ok? ce)
     (precision-emitter-term ce))]
  #:property prop:sequence
  )

(define (cfpe cf)
  (let-values (((it gen) (sequence-generate* cf)))
    (precision-emitter #f '((1 0)(0 1)) '(0 1) (kar it) gen)))

(ce-struct base-emitter (term state inner-term generator transformer)
  #:methods gen:consumer-emitter
  [(define (->term ce)
     (base-emitter-term ce))
   (define (->next ce)
     ; take the term already pulled by the sequence out of the state matrix
     ; update the generator
     ; start looping to generate the next term
     (struct-values (term state inner-term generator transformer) ce)
     (if term
         (if (not state)
             (base-emitter #f #f #f #f #f)
             (let ((state* (matrix* (transformer term)
                                    state)))
               (consume (base-emitter term state* inner-term generator transformer) (consume-limit))))
         (consume ce (consume-limit))))
   (define (->finish ce)
     (struct-values (term state) ce)
     (let ((n (apply quotient (map car state))))
       (if (zero? n)
           (if term
               (base-emitter #f #f #f #f #f)
               (base-emitter n #f #f #f #f))
           (base-emitter n #f #f #f #f))))
   (define (init ce)
     (consume ce (consume-limit)))
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
     (struct-values (term state inner-term generator transformer) ce)
     (if (not inner-term)
         (let ((maybe-t (emit-check ce)))
           (if maybe-t
               (base-emitter maybe-t state inner-term generator transformer)
               #;(base-emitter #f #f #f #f #f)
               (->finish ce)
               ))
         (if (zero? limit)
             (begin (display "consume limit reached" (current-error-port))
                    (->finish ce))
             (let* ((state* (matrix* state (lift inner-term))))
               (let-values (((it gen) (generator)))
                 (let ((maybe-t (emit-check (base-emitter term state* inner-term generator transformer))))
                   (if maybe-t
                       (base-emitter maybe-t state* (kar it) gen transformer)
                       (consume (base-emitter term state* (kar it) gen transformer)
                                (sub1 limit)))))))))
   (define (emit-check ce)
     (struct-values (term state inner-term) ce)
     (let-values (((a b) (apply values (car state)))
                  ((c d) (apply values (cadr state))))
       ; base terms are from 0 to +/-infty so the check is different
       (and (not (zero? c))
            (not (zero? d))
            (let ((q1 (quotient a c))
                  (q2 (quotient b d)))
              (if (or (and (= q1 q2)) (not inner-term))
                  (if (zero? a) #f q1)
                  #f)))))
   (define (emit-ok? ce)
     (base-emitter-term ce))]
  #:property prop:sequence)

(define (cfbe cf base)
  (let-values (((it gen) (sequence-generate* cf)))
    (base-emitter #f '((1 0)(0 1)) (kar it) gen (base-transformer base))))

(module+ test
  (check-equal? (pull (cfbe (rat 1/10) 2) 20)
                '(0 0 0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0))
  (check-equal? (pull (cfbe (rat 3/2) 3) 20)
                (build-list 20 (λ(x) 1))))

(ce-struct simple-arithmetic-2 (term state x xgen y ygen)
  #:methods gen:consumer-emitter
  [(define (state->values state)
     (apply values (append (car state) (cadr state))))
   (define (->term ce)
     (simple-arithmetic-2-term ce))
   (define (->finish state)
     ; we reached a consume limit, so consider x,y->infty
     (let ((ts (map car state)))
       (if (zero? (cadr ts))
           (simple-arithmetic-2 #f #f #f #f #f #f)
           (rational->cf (round (apply / ts))))))
   (define (->next ce)
     ; take the term already pulled by the sequence out of the state matrix
     ; start looping to generate the next term
     (struct-values (term state x xgen y ygen) ce)
     (if term
         (let ((state* (matrix* (standard-transformer term)
                                state)))
           (consume (simple-arithmetic-2 term state* x xgen y ygen) (consume-limit)))
         (consume ce (consume-limit))))
   (define (init ce)
     (consume ce (consume-limit)))
   (define (consume-which? ce)
     ; see if the state matrix has different limits for combinations
     ;
     ; consuming a term matrix X for x is like the combination
     ;   ((a c)(e g)).X and ((b d)(f h)).X (which we'll call L and R matrices)
     ; consuming a term matrix T for y is like the combination
     ;   ((a b)(e f)).Y and ((c d)(g h)).Y
     ; so we have to check that each of these submatrices agree in their
     ;   terms for x->1 or infty and y->1 or infty
     ; default: just consume X
     (struct-values (term state) ce)
     (define-values (a b c d e f g h)
       (state->values state))
     (define get-y?
       (let ((Lid e)
             (L1d (+ e f))
             (Rid g)
             (R1d (+ g h)))
         (or (ormap zero? (list Lid L1d Rid R1d))
             (not (= (sign Lid) (sign L1d)))
             (not (= (sign Rid) (sign R1d)))
             (not (= (quotient a Lid)
                     (quotient (+ a b) L1d)))
             (not (= (quotient c Rid)
                     (quotient (+ c d) R1d)))))
       )
     (define get-x?
       ;((a c)(e g)).X and ((b d)(f h)).X
       (let ((Lid e)
             (L1d (+ e g))
             (Rid f)
             (R1d (+ f h)))
         (or (ormap zero? (list Lid L1d Rid R1d))
             (not (= (sign Lid) (sign L1d)))
             (not (= (sign Rid) (sign R1d)))
             (not (= (quotient a Lid)
                     (quotient (+ a c) L1d)))
             (not (= (quotient b Rid)
                     (quotient (+ b d) R1d)))))
       )
     (cond ((zero? h) ; both X and Y update h so it is important to catch this
                      ; otherwise you can get into an infinite loop of getting just X or Y
            'xy)
           ((and get-x? get-y?)
            'xy)
           (get-x? 'x)
           (get-y? 'y)
           (else
            (let ((quotients (map quotient (list a b c d) (list e f g h))))
              (if (not (apply = quotients))
                  'xy
                  (let ((q (car quotients)))
                    (if (negative? q)
                        (if (or (not term) (zero? term))
                            (sub1 q)
                            'xy)
                        q)))))))
   (define (emit-ok? ce)
     (->term ce))
   (define (consume ce limit)
     ; if one generator died, then switch to simple arithmetic
     ; if we reached our limit for consume attempts, finish the rational
     ; otherwise
     ; determine which inner term to consume and apply it to the state matrix
     ; see if we can emit a term
     ; if so, put the term in the field
     ; otherwise, consume again
     (struct-values (term state x xgen y ygen) ce)
     (cond ((zero? limit)
            (displayln "consume limit reached" (current-error-port))
            (->finish state))
           ((not x)
            ; x went to infinity, putting axy + bx + cy + d to ay+b
            (let-values (((a b c d e f g h) (state->values state)))
              (simple-arithmetic #f (list (list a b) (list e f)) y ygen)))
           ((not y)
            ; y went to infinity, putting axy + bx + cy + d to ax+c
            (let-values (((a b c d e f g h) (state->values state)))
              (simple-arithmetic #f (list (list a c) (list e g)) x xgen)))
           (else
            (let ((maybe-t (consume-which? ce))) 
              (case maybe-t
                ((x)
                 (consume (consume-x ce) (sub1 limit)))
                ((y)
                 (consume (consume-y ce) (sub1 limit)))
                ((xy)
                 (consume (consume-xy ce) (sub1 limit)))
                (else
                 (simple-arithmetic-2 maybe-t state
                                      x xgen
                                      y ygen)))))))
   (define (consume-x ce)
     ; consuming a term matrix X for x is like the combination
     ;   ((a c)(e g)).X and ((b d)(f h)).X (which we'll call L and R matrices)
     (define (lift-x t)
       (if (number? t)
           (list (list t 0 1 0)
                 (list 0 t 0 1)
                 (list 1 0 0 0)
                 (list 0 1 0 0))
           (let-values (((p q) (apply values (car t)))
                        ((r s) (apply values (cadr t))))
             (list (list p 0 q 0)
                   (list 0 p 0 q)
                   (list r 0 s 0)
                   (list 0 r 0 s)))))
     (struct-values (term state x xgen y ygen) ce)
     (let ((state* (matrix* state (lift-x x))))
       (let-values (((x* xgen*) (xgen)))
         (simple-arithmetic-2 term state* (kar x*) xgen* y ygen))))
   (define (consume-y ce)
     (define (lift-y t)
       (if (number? t)
           (list (list t 1 0 0)
                 (list 1 0 0 0)
                 (list 0 0 t 1)
                 (list 0 0 1 0))
           (let-values (((p q) (apply values (car t)))
                        ((r s) (apply values (cadr t))))
             (list (list p q 0 0)
                   (list r s 0 0 )
                   (list 0 0 p q)
                   (list 0 0 r s)))))
     (struct-values (term state x xgen y ygen) ce)
     (let ((state* (matrix* state (lift-y y))))
       (let-values (((y* ygen*) (ygen)))
         (simple-arithmetic-2 term state* x xgen (kar y*) ygen*))))
   (define (consume-xy ce)
     (consume-y (consume-x ce)))]
  #:property prop:sequence)

(define (cfce2 x y state)
  (let-values (((x xgen) (sequence-generate* x))
               ((y ygen) (sequence-generate* y)))
    (simple-arithmetic-2 #f state (kar x) xgen (kar y) ygen)))