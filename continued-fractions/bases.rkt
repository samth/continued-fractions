#lang racket/base

(require racket/local racket/dict
         "private/consumer-emitters.rkt")

(provide representation make-representation
         rep?
         digits
         continued-fraction->string
         ->string
         ->number
         string->string)

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
    (or (continued-fraction? cf)
        (and (list? cf)
             (andmap (λ(n) (and (integer? n)
                                (exact? n)))
                     cf))))
  (when (not cf-like-list?)
    (error 'continued-fraction->string
           "Expected a continued fraction or list of integers: ~a" cf))
  (define B (get-base))
  (define base (vector-length (get-base)))
  (define cf-list (if (continued-fraction? cf)
                      (for/list ((t (base-emit cf base))
                                 (i (in-range (add1 (digits))))) ; add1 because of leading integer-part
                        t)
                      cf))
  (define radix (rep-radix (representation)))
  (define i-part (integer->string (abs (car cf-list))))
  (define f-part (map (λ(n) (number->rep 'continued-fraction->string n))
                      (map abs (cdr cf-list))))
  (string-append (if (ormap negative? cf-list) (format "~a" (get-negate)) "")
                 i-part
                 (if radix
                     (if (null? f-part)
                         ""
                         (list->string (cons radix f-part)))
                     (list->string f-part))))

(define (make-reverse-ref)
  (define dict (make-hasheqv))
  (for ((t (in-vector (get-base)))
        (i (in-naturals)))
    (dict-set! dict t i))
  (λ(c)
    (dict-ref dict c c)))

(define (->number s)
  (define radix (get-radix))
  (define B (vector-length (get-base)))
  (define ref (make-reverse-ref))
  (define (loc->ipart+loc loc) ; removing the integer part, assuming negative is out
    (for/fold ((ipart '())
               (remainder loc))
              ((c loc)
               #:break (eqv? c radix))
      (values (cons (ref c) ipart)
              (cdr remainder))))
  (define (loc->fpart loc)
    (for/fold ((fpart '()))
              ((c loc))
      (cons (ref c) fpart)))
  (and (string? s)
       (let* ((loc (string->list s))
              (neg? (eqv? (car loc) (get-negate))))
         (let-values (((ipart loc*) (loc->ipart+loc (if neg? (cdr loc) loc))))
           (and (andmap number? ipart)
                (let ((fpart (if (null? loc*) '() (loc->fpart (cdr loc*)))))
                  (and (andmap number? fpart)
                       (let ((fpart* (if (null? fpart)
                                         (list 0)
                                         fpart)))
                         (let ((i (foldr (λ(t a) (+ t (* B a))) 0 ipart))
                               (f (foldr (λ(t a) (+ t (* B a))) 0 fpart*)))
                           (let ((n (+ i (/ f (expt B (length fpart*))))))
                             (if neg?
                                 (* n -1)
                                 n)))))))))))

(define (->string n)
  (continued-fraction->string (rational->cf n)))

(define (string->string s rep*)
  (let ((n (->number s)))
    (and n
         (rep? rep*)
         (parameterize ((representation rep*))
           (->string n)))))

(module+ test
  (require rackunit)
  (check-equal? (->string (+ 12 1/2))
                "12.5")
  (parameterize ((representation
                  (make-representation #:terms "ab")))
    (check-equal? (->string (+ 4 1/2))
                  "baa.b"))
  (parameterize ((representation
                  (make-representation #:radix #\/
                                       #:terms "0123456789abcdef")))
    (let ((n 123))
      (check-equal? (->string n)
                    (number->string n 16)))))