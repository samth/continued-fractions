#lang racket/base
(require (for-syntax racket/base)
         (only-in math/number-theory integer-root)
         racket/sequence
         "sequence-utils.rkt"
         "general-continued-fractions.rkt"
         "consumer-emitters.rkt")

(provide phi-cf phi-terms
         pi-cf pi-terms
         exp-cf exp-terms
         ln-cf log-cf
         sine-cf sine-terms cosine-cf cosine-terms tangent-cf tangent-terms
         hyperbolic-sine-cf hyperbolic-cosine-cf hyperbolic-tangent-cf
         expt-cf expt-terms
         (all-from-out "consumer-emitters.rkt"))

(define-syntax (define-continued-fraction stx)
  (let ((hyphenate (λ(base end) (string->symbol (format "~a-~a" base end)))))
    (syntax-case stx ()
      ((_ (name arg ...) body ...)
       (with-syntax ((t-name (datum->syntax stx
                                            (hyphenate (syntax->datum #'name) 'terms)))
                     (cf-name (datum->syntax stx
                                             (hyphenate (syntax->datum #'name) 'cf))))
         #'(begin
             (define (t-name arg ...)
               (when (and (not (null? (list arg ...)))
                          (andmap (λ(a) (not (exact? a))) (list arg ...)))
                 (error 't-name "Exact arguments required."))
               body ...)
             (define (cf-name arg ...)
               (when (and (not (null? (list arg ...)))
                          (andmap (λ(a) (not (exact? a))) (list arg ...)))
                 (error 'cf-name "Exact arguments required."))
               (cfce1 (t-name arg ...) '((1 0)(0 1))))))))))
      #;((_ name body)
       (with-syntax ((t-name (datum->syntax stx
                                            (hyphenate (syntax->datum #'name) 'terms)))
                     (cf-name (datum->syntax stx
                                             (hyphenate (syntax->datum #'name) 'cf))))
         #'(begin
             (define t-name body)
             (define cf-name (cfce1 t-name '((1 0)(0 1)))))))

(define odds (enumerate-naturals 0 (λ(x) (add1 (* 2 x)))))
(define evens (enumerate-naturals 0 (λ(x) (* 2 x))))
(define 1... (endless-values 1))
(define 0... (endless-values 0))

(define-continued-fraction (phi) 1...)

(module+ test
  ; a note for tests: not all convergents will match the output of a continued fraction
  ; because an emission might end in ... a 1 == ... (a+1)
  ; but expanding the convergent the actual terms from the continued fraction WON'T respect this
  ; and will just end in ... a+1 because we've freezed the emission, not terminated it
  (require (for-syntax syntax/parse)
           rackunit)
  (define (pull cf . terms)
    (if (null? terms)
        (for/list ((t cf)) t)
        (for/list ((t cf) (i (in-range (car terms)))) t)))
  (define-syntax (check-convergents stx)
    (syntax-parse stx
      [(_ cf-constructor (val:number ...) (r:number ...))
       #'(parameterize ((precision +inf.0)
                        (consume-limit 500))
           (for ((v (list val ...))
                 (rational (list r ...)))
             (let ((ref (pull (rat rational))))
               #;(printf "checking (~a ~a):\n " 'cf-constructor v)
               (check-equal? (pull (cf-constructor v) (length ref))
                             ref
                             (format "(~a ~a)" 'cf-constructor v)))))]))
  (let ((sqrt-5 (sequence-append (list 2) (endless-values 4))))
    (let ((phi-calc (cfce1 sqrt-5 '((1 1)(0 2)))))
      (parameterize ((precision +inf.0))
        (check-equal? (pull phi-calc 128)
                      (pull (phi-cf) 128)
                      "(/ (+ 1 (sqrt 5)) 2) = phi")))))

(define-continued-fraction (pi)
  ; there are many general continued fractions for pi
  ; most converge extremely slowly, just like many of the products and series
  ; by experimentation with the ones I found this is the easiest/fastest
  (make-general-cf (sequence-append (list 0) odds)
                   (sequence-append (list 4) (enumerate-naturals 1 (λ(x) (* x x))))))

(module+ test
  (parameterize ((precision +inf.0))
    (let ((ref (pull (rat 1001262030186123788147677117546011454046175300701051137698775095496/318711602868696242700351405960501012107407621461407582086159833951))))
      (check-equal? (pull (pi-cf) (length ref))
                    ref
                    "Continued fraction for pi"))))


(define-continued-fraction (exp x)
  (define-values (p q) (values (numerator x) (denominator x)))
  (define -p (- p))
  (define numerators (interleave (endless-values p) (endless-values -p)))
  (define denominators (sequence-append (list 1)
                                        (interleave (sequence-map (λ(t) (* t q)) odds)
                                                    (endless-values 2))))
  (make-general-cf denominators numerators))

(module+ test
  (check-convergents exp-cf
                     (4/3 2/3 -4/3 101/23 23/101)
                     (334793927275157099977828070462712014573208871193375356742100358854/88250721088256169900164260299781937337302784521501669725657881353
                      2495365459898803372232735224760576195990177535799212229935666489111023275430878820402070330878149823014936140648193640458972762008996393478408235432429355692154938391542709936/1281163345354682677043664082719994498938512266769570062429490864533600964598790852020670583820556643361608843634288769557008584560429674272852670181434859989421083378106058591
                      88250721088256169900164260299781937337302784521501669725657881353/334793927275157099977828070462712014573208871193375356742100358854
                      424341140432488081310930105724962594875896975065061628279766603099342/5255280393630844818098354474555432361881446816634028387976716119347
                      88535850947358374997591564469700139137905865620869096021706911141/70505082058619621387977193310299292277413734534000392321660004996)))

(define (ln-cf x)
  (define-continued-fraction (ln-base z) ;1+x/y
    (define x (numerator (sub1 z)))
    (define y (denominator (sub1 z)))
    (define numerators* (sequence-map (λ(t) (* x t)) (in-naturals 1)))
    (define numerators (sequence-append (list x)
                                        (interleave numerators* numerators*)))
    (define denom-1 (sequence-map (λ(t) (* y t)) odds))
    (define denom-2 (endless-values 2))
    (define denominators (sequence-append (list 0)
                                          (interleave denom-1 denom-2)))
    (make-general-cf denominators numerators))
  ; calculating logs means we need to turn multiplication into addition
  ; find values a,b,n such that x = b*a^n
  ; this is easy to do if we assume a is something nice like 5/4
  ; find the "rational root" n and solve for b
  (define (decompose x)
    (define (get-exponent z r)
      (if (< z 2)
          0
          (+ 1 (get-exponent (/ z r) r))))
    (define a 5/4)
    (define n (get-exponent x a))
    (define b (/ x (expt a n)))
    (values a b n))
  (cond ((<= x 0)
         (error 'ln-cf "Real logarithm undefined for (<= 0 x): ~a" x))
        ((< 0 x 2)
         (ln-base-cf x))
        (else
         (let-values (((a b n) (decompose x)))
           ; now we have ln(x) => ln(b*a^n) = n*ln(a) + ln(b)
           (cfce2 (ln-base-cf a) (ln-cf b) `((0 ,n 1 0)(0 0 0 1)))))))

(define (log-cf base x)
  (cfce2 (ln-cf x) (ln-cf base) '((0 1 0 0)(0 0 1 0))))


(define-continued-fraction (cosine x)
  ; derived from maclaurin expansion for cosine
  (define x^2 (* x x))
  (define x-factor (sequence-map (λ(t) (/ x^2 (* t (add1 t)))) odds))
  (define p^2 (* (numerator x) (numerator x)))
  (define q^2 (* (denominator x) (denominator x)))
  (define pq^2 (* p^2 q^2))
  (define q-factor (sequence-map (λ(t) (* q^2 t (add1 t))) odds))
  (define denom-terms (sequence-map (λ(t) (- (* q^2 t (add1 t)) p^2)) odds))
  (define denominators (sequence-append (list 0 1) denom-terms))
  (define numerators (sequence-append (list 1 p^2)
                                      (sequence-map (λ(t) (* t (add1 t) pq^2)) odds)))
  (make-general-cf denominators numerators))


(module+ test
  (check-convergents cosine-cf
                     (-3 -2 -1 -1/2 1/2 1 2 3 #e3.14159)
                     (-3832886903173145214001860938733765471/3871632276340447333499021546653201936
                      -212722238127196433807452947428576656/511171104632675990006918853207168139
                      374466817986325582540322462076924939/693069072479053728669596379009533353
                      431735640793019922212963554989161/491960140893219088642441135849416
                      431735640793019922212963554989161/491960140893219088642441135849416
                      374466817986325582540322462076924939/693069072479053728669596379009533353
                      -212722238127196433807452947428576656/511171104632675990006918853207168139
                      -3832886903173145214001860938733765471/3871632276340447333499021546653201936
                      -371358464509224817419461518690762886253037303741941771423351626380692879575/371358464510532284936028335737601114085292727320512689881362698794085664844)))

(define-continued-fraction (sine x)
  ; derived from maclaurin expansion for sin
  (define-values (p q) (values (numerator x) (denominator x)))
  (define-values (p^2 q^2)
    (let ((^ (λ(x) (* x x))))
      (values (^ p) (^ q))))
  (define pq^2 (* p^2 q^2))
  (define numerators (sequence-append (list (* p q) pq^2)
                                      (sequence-map (λ(t) (* t (add1 t) pq^2))
                                                    (sequence-tail evens 1))))
  (define denominators (sequence-append (list 0 q^2)
                                        (sequence-map (λ(t) (- (* t (add1 t) q^2) p^2))
                                                      (sequence-tail evens 1))))
  (make-general-cf denominators numerators))

(module+ test
  (check-convergents sine-cf
                     (1/2 1 2 3 #e3.14159)
                     (275816470114301133993435959012608376/575306169373687849690896950134734835
                      355802506861992393238111455156268/422833957778378150059786358909915
                      259394244645214656849237862877324387/285268864722018224410836519808739871
                      44495120319096667086302326738480/315299870874585974288758052940061
                      11447658691968077564818351366108437206267352216009499108123789/4314027255136030769842614312289943645600038425247482978622205589867)))

(define-continued-fraction (tangent x)
  ; wikipedia with modifications for rational x = p/q
  (define-values (p q) (values (numerator x) (denominator x)))
  (define q^2 (* q q))
  #;(define -pq^2 (- (* p p q q)))
  #;(define numerators (sequence-append (list (* p q))
                                      (endless-values -pq^2)))
  #;(define denominators (sequence-append (list 0)
                                        (sequence-map (λ(t) (* t q^2)) odds)))
  (define numerators (sequence-append (list (* p q)) (endless-values (- (* p p)))))
  (define denominators (sequence-append (list 0)
                                        (interleave (enumerate-naturals 0 (λ(t) (* (add1 (* 4 t)) q^2)))
                                                    (enumerate-naturals 1 (λ(t) (sub1 (* 4 t)))))))
  (define force-terms
    (let* ((p^2 (* p p))
           (q^2 (* q q))
           (q (quotient p^2 q^2))
           (q* (if (even? q) q (add1 q))))
      (let ((n (/ q* 2)))
        (add1 n))))
  (make-general-cf denominators numerators #:force force-terms))

(module+ test
  (check-convergents tangent-cf
                     (-1/2 1/2 1 5/2 2 3 #e3.14159)
                     (-1465545425891750856804942488959142506089867910908375963038693842332397593609124125167853402149/2682662907706696070023721365182286397983711950195397695763956830248544492480107937292771272649
                      1465545425891750856804942488959142506089867910908375963038693842332397593609124125167853402149/2682662907706696070023721365182286397983711950195397695763956830248544492480107937292771272649
                      230471798175949083975024214753114071043451384730708680916386734503142668417021899/147984239789884254649266143916384887026335861964844504679327328925468649325805575
                      -152809391954634168381330486787252470944511756947075/204558006527366474844505404809806627690609112019141
                      -1376971776809483938350194622016064983302802717059659/630181535797765671498018311443206258146380886233718
                      -867428969542549392209718548625077679438507587616/6085233291771597846360884787273513886706011495397
                      -45806084575273652941868673813955660794636846952959608808300173/17261931249465659999654074509311050464140611102949241543524859831200))
  (let ((x #e3.14159265358979323846264338327))
    (check-equal? (pull (cfce2 (sine-cf x) (cosine-cf x) '((0 1 0 0)(0 0 1 0))) 50)
                  (pull (cfce1 (tangent-cf x) '((1 0)(0 1))) 50)
                  "(/ (sin x) (cos x)) = (tan x)")
    (check-equal? (pull (cfce2 (sine-cf (- x)) (cosine-cf (- x)) '((0 1 0 0)(0 0 1 0))) 50)
                  (pull (tangent-cf (- x)) 50)
                "(/ (sin -x) (cos -x)) = (tan -x)")
    (check-equal? (pull (cfce2 (cosine-cf x) (tangent-cf x) '((1 0 0 0) (0 0 0 1))) 50)
                  (pull (sine-cf x) 50)
                  "(* (tan x) (cos x)) = (sin x)")
    ))

(define (rational-root x n)
  ; due to the way truncation of elements of a rational works
  ; you can't just do (integer-root numerator) (integer-root denominator)
  ; there is at most one rational for integer roots, but an infinite number
  ; of them for rational roots
  ; this dumb method is the best I could come up with
  (let ((p (numerator x))
        (q (denominator x)))
    (let loop ((P (integer-root p n))
               (Q (integer-root q n)))
      (let ((y (/ P Q)))
        (if (positive? (- x (expt y n)))
            y
            (if (> P 1)
                (loop (sub1 P) Q)
                (loop P (add1 Q))))))))

(define (quadratic-cf +? a b c)
  (define discriminant (- (* b b) (* 4 a c)))
  (cond ((zero? a)
         (rat (/ (- c) b)))
        ((zero? b)
         (expt-cf (/ (- c) a) 1/2))
        ((zero? c)
         (rat (/ (- b) a)))
        ((negative? discriminant)
         (error 'solve-quadratic "Complex roots"))
        (else
         (cfce1 (expt-terms discriminant 1/2) `((,(if (eq? +? '+) 1 -1) ,(- b))
                                               (0 ,(* 2 a)))))))

(define-continued-fraction (expt base exponent)
  ; ridiculously complicated form in programming that looks nice on paper
  ; reference "General Method for Extracting Roots using (Folded) Continued Fractions" by Manny Sardina
  ; modified so that the base and exponent rational terms in the continued fraction have been replaced
  ;   with integers
  ;   this form assumes the exponent is less than 1
  ;   if it is greater than one, it applies the power and then takes the root
  (define-values (m n)
    (values (numerator exponent) (denominator exponent)))
  (cond ((and (negative? exponent)
              (even? n))
         (error 'expt-cf "Complex root: ~a^(~a/~a)" base n m))
        ((negative? exponent)
         (expt-terms (/ base) (abs exponent)))
        ((= 1 exponent)
         (rat base))
        ((= -1 exponent)
         (rat (/ base)))
        ((and (positive? m) (positive? n)
              (> m n))
         (expt-terms (expt base m) (/ n)))
        ((and (positive? m) (positive? n)
              (> n m))
         (let* ((base* (rational-root base n))
                (P (numerator base*))
                (P^m (expt P m))
                (Q (denominator base*))
                (Q^n (expt Q n))
                (Q^m (expt Q m))
                (r/s (- base (expt (/ P Q) n)))
                (r (numerator r/s))
                (s (denominator r/s))
                (N (- n m))
                (snP^N (* s n (expt P N)))
                (2P^m (* 2 (expt P m)))
                (rQ^n (* r (expt Q n))))
           (let-syntax ((U (syntax-rules ()
                             ((_ i) (- (* i n) m))))
                        (V (syntax-rules ()
                             ((_ i) (+ (* i n) m)))))
             (let ((reg-num-1 (sequence-map (λ(i) (* (U i) rQ^n)) (in-naturals 1)))
                   (reg-num-2 (sequence-map (λ(i) (* (V i) rQ^n)) (in-naturals 1)))
                   (reg-denom-1 (sequence-map (λ(i) (* i snP^N)) odds))
                   (reg-denom-2 (endless-values 2P^m)))
               (general-cf (sequence-append (list 1 P^m)
                                            (interleave reg-denom-1 reg-denom-2))
                           (sequence-append (list 0 (* r m Q^n))
                                            (interleave reg-num-1 reg-num-2))
                           (sequence-append (list 0) 1...)
                           (sequence-append (list Q^m) 0...))))))))

(module+ test
  (check-convergents (λ(exp) (expt-cf 7933/17417 exp)) 
                     (1/2 2/3 3/4 4/5 5/6 6/5 5/4 4/3 3/2)
                     (1404223170283508601133126886570380382/2080674207431828538335112883732446055
                      28000492469076778327934374556785/47299469312293507579108688166604
                      109353407790132643322540284128054/197235118205752304395154061730219
                      200457755519626593060794388902867/376055185297397138050290078602722
                      513500225795074269852092805532507/988903438248265096747734679273936
                      7711166779785002175007977532781/19813590064386972429987611049660
                      1469658256134917849332377532249424/3927681206591959147125407632707333
                      1407443409849023300575819720204/4016170470478220866914014576747
                      1328091715578514295829021322421/4320479153423892356017516007384))
  (check-equal? (pull (cfce2 (expt-cf 2 2/3) (expt-cf 1/3 2/3) '((1 0 0 0)(0 0 0 1))) 64)
                (pull (expt-cf 2/3 2/3) 64)
                "(* (expt 2 2/3) (expt 1/3 2/3)) = (expt 2/3 2/3)"))

(define (hyperbolic-sine-cf x)
  ;(e^x - e^-x)/2
  (cfce2 (exp-cf x) (exp-cf (- x)) '((0 1 -1 0)(0 0 0 2))))
(define (hyperbolic-cosine-cf x)
  ;(e^x + e^-x)/2
  (cfce2 (exp-cf x) (exp-cf (- x)) '((0 1 1 0)(0 0 0 2))))
(define (hyperbolic-tangent-cf x)
  ;(e^x - e^-x)/(e^x + e^-x)
  (cfce2 (exp-cf x) (exp-cf (- x)) '((0 1 -1 0)(0 1 1 0))))

(define (to-file cf num-terms)
  (local-require racket/gui/base)
  (let ((f (get-file)))
    (when f
      (with-output-to-file f
        (λ()
          (for ((t (cfbe cf 26))
                (i (in-range num-terms)))
            (display (integer->char (+ 97 t)))))
        #:mode 'text #:exists 'replace))))

(provide to-file)
