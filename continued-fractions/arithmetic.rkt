#lang racket/base
(require racket/list
         "private/consumer-emitters.rkt")

(provide cf+ cf- cf* cf/)



(define-syntax-rule (define-binop (name L R) [both-numbers first-number second-number both-cf])
  (define (name L R)
    (cond ((and (exact-integer? L) (exact-integer? R)) both-numbers)
          ((and (exact-integer? L) (consumer-emitter? R)) first-number)
          ((and (consumer-emitter? L) (exact-integer? R)) second-number)
          ((and (consumer-emitter? L) (consumer-emitter? R) both-cf))
          (else
           (error 'name
                  "Expected exact integers or continued fractions:\n\t~a\n\t~a"
                L R)))))
  
(define-binop (bin+ a b)
  [(rat (+ a b))
   (cfce1 b `((1 ,a)(0 1)))
   (cfce1 a `((1 ,b)(0 1)))
   (cfce2 a b '((0 1 1 0)(0 0 0 1)))])
  
(define-binop (bin* a b)
  [(rat (* a b))
   (cfce1 b `((1 ,a)(0 1)))
   (cfce1 a `((1 ,b)(0 1)))
   (cfce2 a b '((1 0 0 0)(0 0 0 1)))])

(define-binop (bin- a b)
  [(rat (- a b))
   (cfce1 b `((-1 ,a)(1 0)))
   (cfce1 a `((1 ,(- b))(1 0)))
   (cfce2 a b `((0 1 -1 0)(0 0 0 1)))])

(define-binop (bin/ a b)
  [(rat (/ a b))
   (cfce1 b `((0 ,a)(1 0)))
   (cfce1 a `((1 0)(0 ,b)))
   (cfce2 a b `((0 1 0 0)(0 0 1 0)))])

(define (cf+ . ts)
  (define L (length ts))
  (cond ((zero? L)
         0)
        ((not (andmap (λ(t) (or (consumer-emitter? t) (integer? t))) ts))
         (error 'minus "Expected continued fractions and integers."))
        ((= 1 L)
         (bin+ (car ts) 0))
        (else
         (let ((cfs (filter consumer-emitter? ts))
               (ns (filter integer? ts)))
           (foldl bin+ (apply + ns) cfs)))))

(define (cf- . ts)
  (define L (length ts))
  (cond ((zero? L)
         (error 'minus "minus requires at least one argument."))
        ((not (andmap (λ(t) (or (consumer-emitter? t) (integer? t))) ts))
         (error 'minus "Expected continued fractions and integers."))
        ((= 1 L)
         (bin- 0 (car ts)))
        (else
         (bin- (car ts) (apply cf+ (cdr ts))))))

(define (cf* . ts)
  (define L (length ts))
  (cond ((zero? L)
         1)
        ((not (andmap (λ(t) (or (consumer-emitter? t) (integer? t))) ts))
         (error 'minus "Expected continued fractions and integers."))
        ((= 1 L)
         (car ts))
        (else
         (foldl bin* (car ts) (cdr ts)))))

(define (cf/ . ts)
  (define L (length ts))
  (cond ((zero? L)
         (error 'divide "divide requires at least one argument."))
        ((not (andmap (λ(t) (or (consumer-emitter? t) (integer? t))) ts))
         (error 'minus "Expected continued fractions and integers."))
        ((= 1 L)
         (bin/ 1 (car ts)))
        (else
         (bin/ (car ts) (apply cf* (cdr ts))))))