#lang racket/base
(require racket/list
         )
(provide matrix?
         matrix+ matrix*
         column
         transpose
         )

(define (matrix? m)
  (andmap (λ(row) (and (list? row) (andmap number? row))) m))

(define (zeros n)
  (build-list n (λ(x) 0)))

(define (add m1 m2)
  (if (and (= (length m1) (length m2))
           (= (length (first m1)) (length (first m2))))
      (map (λ(r1 r2) (map + r1 r2)) m1 m2)
      (error 'matrix+ "Addition size mismatch:\n\t~a\n\t~a" m1 m2)))

(define (matrix+ . ms)
  (cond ((empty? ms)
         (error 'matrix+ "Requires at least one argument."))
        ((empty? (rest ms))
         (first ms))
        (else
         (foldr add (first ms) (rest ms)))))

(define (transpose matrix)
  (if (empty? (first matrix))
      empty
      (cons (map first matrix)
            (transpose (map rest matrix)))))

(define (column vector)
  (transpose (list vector)))

(define (times m1 m2)
  (define (sum list) (apply + list))
  (cond ((and (number? m1) (number? m2))
         (* m1 m2))
        ((number? m1)
         (if (= 1 m1)
             m2
             (map (λ(x) (map (λ(y) (* m1 y)) x)) m2)))
        ((number? m2)
         (if (= 1 m2)
             m1
             (map (λ(x) (map (λ(y) (* m2 y)) x)) m1)))
        ((not (= (length (first m1)) (length m2)))
         (error 'matrix* "Improper dimensions:\n\t~a\n\t~a" m1 m2))
        (else 
         (let ((m2 (transpose m2)))
           (map (λ(x) (map (λ(y) (sum (map * x y))) m2)) m1)))))

(define (matrix* . ms)
  (cond ((empty? ms)
         (error 'matrix* "Not a unary operation"))
        ((empty? (rest ms))
         (first ms))
        ((empty? (rest (rest ms)))
         (times (first ms) (second ms)))
        (else
         (foldl (λ(term accum) (times accum term))
                (first ms) (rest ms)))))

(define (matrix-expt m ^n)
  (cond ((= 0 ^n)
         '((1 0) (1 0)))
        ((even? ^n)
         (matrix-expt (matrix* m m) (/ ^n 2)))
        (else (matrix* m (matrix-expt m (sub1 ^n))))))

(define (zero x) 0)

(define (num-zeroes row)
  (cond ((empty? row)
         0)
        ((zero? (first row))
         (+ 1 (num-zeroes (rest row))))
        (else 0)))

(define (make-identity length)
  (let loop ((i length)
             (accum empty))
    (if (zero? i)
        accum
        (loop (sub1 i)
              (cons (append (build-list (sub1 i) zero)
                            (list 1)
                            (build-list (- length i) zero))
                    accum)))))
