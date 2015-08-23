#lang racket/base
(require racket/list
         "private/consumer-emitters.rkt")

(provide plus minus times divide)

(define-syntax-rule (define-binop (name L R) [both-numbers first-number second-number both-cf])
  (define (name L R)
    (cond ((and (integer? L) (integer? R)) both-numbers)
          ((and (integer? L) (consumer-emitter? R)) first-number)
          ((and (consumer-emitter? L) (integer? R)) second-number)
          ((and (consumer-emitter? L) (consumer-emitter? R) both-cf))
          (else
           (error 'name
                  "Expected numbers or continued fractions:\n\t~a\n\t~a"
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

(define (plus . ts)
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

(define (minus . ts)
  (define L (length ts))
  (cond ((zero? L)
         (error 'minus "minus requires at least one argument."))
        ((not (andmap (λ(t) (or (consumer-emitter? t) (integer? t))) ts))
         (error 'minus "Expected continued fractions and integers."))
        ((= 1 L)
         (bin- 0 (car ts)))
        (else
         (bin- (car ts) (apply plus (cdr ts))))))

(define (times . ts)
  (define L (length ts))
  (cond ((zero? L)
         1)
        ((not (andmap (λ(t) (or (consumer-emitter? t) (integer? t))) ts))
         (error 'minus "Expected continued fractions and integers."))
        ((= 1 L)
         (car ts))
        (else
         (foldl bin* (car ts) (cdr ts)))))

(define (divide . ts)
  (define L (length ts))
  (cond ((zero? L)
         (error 'divide "divide requires at least one argument."))
        ((not (andmap (λ(t) (or (consumer-emitter? t) (integer? t))) ts))
         (error 'minus "Expected continued fractions and integers."))
        ((= 1 L)
         (bin/ 1 (car ts)))
        (else
         (bin/ (car ts) (apply times (cdr ts))))))