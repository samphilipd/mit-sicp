#lang scheme

(require (planet williams/science/random-source))

(define (sqrt x)
  (define (square a)
    (* a a))
  (define (average a b)
    (/ (+ a b) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
         guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (factorial-recursive n)
  (if (= n 1)
      1
      (* n (factorial-recursive (- n 1)))))

(define (factorial-iterative n)
  (define (fact-iter product counter)
    (if (> counter n)
        product
        (fact-iter (* product counter) (+ counter 1))))
  (fact-iter 1 1))

(define (inc a)
  (+ a 1))

(define (dec a)
  (- a 1))

(define (p+recur a b)
   (if (= a 0)
       b
       (inc (p+recur (dec a) b))))

(define (p+iter a b)
  (if (= a 0)
      b
      (p+iter (dec a) (inc b))))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(define (fib-recur n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-recur (- n 1)) (fib-recur (- n 2))))))

(define (fib-iter n)
  (define (fib-iterative a b count)
    (if (= count 0)
        b
        (fib-iterative (+ a b) a (- count 1))))
  (fib-iterative 1 0 n))

(define (count-change amount)
  (define (first-denomination coin-types)
    (cond ((= coin-types 1) 1)
          ((= coin-types 2) 5)
          ((= coin-types 3) 10)
          ((= coin-types 4) 25)
          ((= coin-types 5) 50)))
  (define (cc amount coin-types)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= coin-types 0)) 0)
          (else (+ (cc amount (- coin-types 1)) (cc (- amount (first-denomination coin-types)) coin-types)))))
  (cc amount 5))

(define (fn-111-recur n)
  (cond ((< n 3) n)
        ((>= n 3) (+ (fn-111-recur (- n 1)) (* 2 (fn-111-recur (- n 2))) (* 3 (fn-111-recur (- n 3)))))))

(define (fn-111-iter n)
  (define (fi nlimit a fna-1 fna-2 fna-3)
    (cond
      ((> a nlimit) fna-1)
      (else (fi nlimit (+ a 1) (+ fna-1 (* 2 fna-2) (* 3 fna-3)) fna-1 fna-2))))
  (cond ((< n 3) n)
        ((>= n 3) (fi n 3 2 1 0))))

(define (pascal row column)
  ;computes an element in pascal's triangle given a row and column index
  (cond
    ((or (= column 1) (= column row)) 1)
    (else (+ (pascal (- row 1) (- column 1)) (pascal (- row 1) column)))))

(define (cube x) (* x x x))
(define (p x)
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b count product)
  (if (= count 0)
      product
      (expt-iter b (- count 1) (* b product))))


(define (expt-recur b n)
  (if (= n 0)
      1
      (* b (expt-recur b (- n 1)))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square a)
  (* a a))

(define (fast-expt b n)
  (cond
    ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
    (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter b n)
  (define (do-fast-expt-iter b n a)
    (cond
      ((= n 0) a)
      ((even? n) (do-fast-expt-iter (square b) (/ n 2) a))
      (else (do-fast-expt-iter b (- n 1) (* a b)))))

  (do-fast-expt-iter b n 1))

(define (fast-* a b)
  (cond
    ((= b 0) 0)
    ((even? b) (fast-* (* 2 a) (/ b 2)))
    (else (+ a (fast-* a (- b 1))))))

(define (fast-*-iterative a b)
  (define (do-fast-*-iter a b result)
    (cond
      ((= b 0) result)
      ((even? b) (do-fast-*-iter (* 2 a) (/ b 2) result))
      (else (do-fast-*-iter a (- b 1) (+ a result)))))
   (do-fast-*-iter a b 0))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (prime? n)
  (if (= n 1)
      #f
      (= n (smallest-divisor n))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond
    ((= times 0) #t)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else #f)))

(define (timed-prime-test n)
  (start-prime-test n (/ (current-milliseconds) 1000.0)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
      (report-prime n (- (/ (current-milliseconds) 1000.0) start-time))
      #f))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  #t)

(define (search-for-primes n max-primes)
  (if (zero? max-primes)
      (display "\nDone")
      (if (timed-prime-test n)
          (search-for-primes (+ 1 n) (- max-primes 1))
          (search-for-primes (+ 1 n) max-primes))))

(define (a^n-congruent-to-a-modulo-n? n)
  (define (congruent-all-a? a n)
    (if (= n a)
        #t
        (and (= (expmod a n n) a) (congruent-all-a? (+ a 1) n))))
  (congruent-all-a? 1 n))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (simpson-term k)
    (define y (f (+ a (* k h))))
    (if (or (= k 0) (= k n))
        y
        (if (even? k)
            (* 2 y)
            (* 4 y))))
  (* (sum-iter simpson-term 0 inc n) (/ h 3)))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-approx n-terms)
  (define (top-term n)
    (if (even? n)
        (+ 2 n)
        (+ 3 n)))
  (define (bottom-term n)
    (if (even? n)
        (+ 3 n)
        (+ 2 n)))
  (define top (product-acc top-term 0 inc n-terms))
  (define bottom (product-iter bottom-term 0 inc n-terms))
  (* 4 (/ top bottom)))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (product-acc term a next b)
  (accumulate-iter * 1 term a next b))

(define (filtered-accumulate combiner null-value filter term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate combiner null-value filter term (next a) next b))
          (filtered-accumulate combiner null-value filter term (next a) next b))))

(define (sum-squares-primes-only a b)
  (filtered-accumulate + 0 prime? square a inc b))
(define (product-relative-primes n)
  (define (relative-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate * 1 relative-prime? identity 1 inc n))

(define (f g)
  (g 2))

(define (average x y)
  (/ (+ x y) 2))

(define (search f neg-point pos-point)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value) (search f neg-point midpoint))
                ((negative? test-value) (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (negative? b-value) (positive? a-value)) (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

(define (fixed-point f first-guess)
  (let ((tolerance 0.00001))
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (define (try guess)
      (display guess)
      (display " => ")
      (let ((next (f guess)))
        (display next)
        (newline)
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define (sqrt-fixed x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (cont-frac n d k)
   (define (divisor n d c)
     (if (= c k)
         (d k)
         (/ (n c) (+ (d c) (divisor n d (+ c 1))))))
   (divisor n d 1))

(define (cont-frac-iter n d k)
  (define (iter result term)
    (if (= term 0)
        result
        (iter (/ (n term) (+ (d term) result)) (- term 1))))
  (iter 0 k))

(define (approx-e k)
  (define (euler-n i) 1)
  (define (euler-d i)
    (if (= (remainder i 3) 2)
        (/ (+ i 1) 1.5)
        1))
  (+ 2 (cont-frac euler-n euler-d k)))


(define (tan-cf x k)
  (define (tan-cf-d i)
    (+ 1 (* 2.0 (- i 1))))
  (define (recur d i)
    (if (= i k)
        (d k)
        (/ (square x) (- (d i) (recur d (+ i 1))))))
  (recur tan-cf-d 1))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-fp-ad x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(define (cube-root-fp-ad x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(define (deriv g)
  (let ((dx 0.00001))
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-fp-general x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt-newt-general x)
  (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(define (double proc)
  (lambda (x) (proc (proc x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smooth f n)
  (repeated smooth n) f)

(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))

(define (nth-root x n)
  (define (log2 x) (/ (log x) (log 2)))
  (define (f y) (/ x (pow y (- n 1))))
  (let ((n-damps (floor (log2 n))))
    (fixed-point ((repeated average-damp n-damps) f) 1.0)))

(define (iterative-improve good-enough? improve)
  (lambda (initial-guess)
    (define (recur guess)
      (if (good-enough? guess)
          guess
          (recur (improve guess))))
    (recur initial-guess)))

(define (sqrt-general x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

;(define (fixed-point-general f)
;  (define (close-enough? v1 v2)
;    (define tolerance 1.e-6)
;    (< (/ (abs (- v1 v2)) v2)  tolerance))
;  ((iterative-improve f close-enough?) first-guess))
