#lang racket/base

(require racket/local
         "private/consumer-emitters.rkt")

(provide representation make-representation
         continued-fraction->string
         rep?
         digits)

(struct rep (radix negate terms)
  #:prefab)

(define default-terms "0123456789")
(define default-radix #\.)
(define default-negate #\-)

(define (non-negative? n)
  (and (number? n)
       (not (negative? n))))

(define (check-unique list)
  (let loop ((parsed '())
             (list list))
    (if (null? list)
        #t
        (let ((head (car list)))
          (if (member head parsed)
              #f
              (loop (cons head parsed)
                    (cdr list)))))))

(define (make-representation #:radix (r default-radix)
                             #:negate (n default-negate)
                             #:terms (s default-terms))
  (when (not (string? s))
    (error "Expected a string containing unique terms."))
  (when (not (char? n))
    (error "Expected a character for negate symbol."))
  (when (and (not r) (not (char? r)))
    (error "Expected a character or #f for the radix point symbol."))
  (define terms (string->list s))
  (when (member n terms)
    (error "Negate symbol cannot be a term."))
  (when (eqv? n r)
    (error "Radix point must differ from the negate symbol."))
  (when (member r terms)
    (error "Radix point symbol cannot be a term."))
  (rep r n (list->vector terms)))

(define representation
  (make-parameter (make-representation)
                  (λ(v)
                    (if (rep? v)
                        v
                        (error 'representation
                               "make-representation procedure required")))))

(define digits
  (make-parameter 10
                  (λ(n)
                    (if (andmap (λ(p) (p n))
                                (list number? exact? non-negative?))
                        n
                        (error 'digits
                               "Expected an exact, non-negative number: ~a" n)))))

(define (get-base)
  (rep-terms (representation)))
(define (get-radix)
  (rep-radix (representation)))
(define (get-negate)
  (rep-negate (representation)))

(define (number->rep caller n)
  (define B (get-base))
  (define L (vector-length B))
  (if (and (not (negative? n)) (exact? n) (integer? n) (< n L))
      (vector-ref B n)
      (error caller
             "Number exceeds representation bounds [0, ~a): ~a" L n)))

(define (integer->string n)
  (define base (vector-length (get-base)))
  (define terms
    (let loop ((n (abs n))
               (ts '()))
      (if (zero? n)
          (if (null? ts) (list 0) ts)
          (let-values (((q r) (quotient/remainder n base)))
            (loop q (cons r ts))))))
  (let ((chars (map (λ(t) (number->rep 'number->string t)) terms)))
    (list->string
     (if (negative? n)
         (cons (rep-negate (representation)) chars)
         chars))))

(define (continued-fraction->string cf)
  (define cf-like-list?
    (or (consumer-emitter? cf)
        (and (list? cf)
             (andmap (λ(n) (and (integer? n)
                                (exact? n)))
                     cf))))
  (when (not cf-like-list?)
    (error 'continued-fraction->string
           "Expected a continued fraction or list of integers: ~a" cf))
  (define B (get-base))
  (define base (vector-length (get-base)))
  (define cf-list (if (consumer-emitter? cf)
                      (for/list ((t (cfbe cf base))
                                 (i (in-range (add1 (digits))))) ; add1 because of leading integer-part
                        t)
                      cf))
  (define radix (rep-radix (representation)))
  (define i-part (integer->string (abs (car cf-list))))
  (define f-part (map (λ(n) (number->rep 'continued-fraction->string n))
                      (map abs (cdr cf-list))))
  (string-append (if (negative? (cadr cf-list)) (format "~a" (get-negate)) "")
                 i-part
                 (if radix
                     (list->string (cons radix f-part))
                     (list->string f-part))))