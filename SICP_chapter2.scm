#lang scheme

(require "./SICP_chapter1.scm")

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; ex 2.1
(define (make-rat n d)
  (define (neg x)
    (- 0 x))
  (let ((g (gcd (abs n) (abs d))))
    (if (or (and (negative? d) (negative? n)) (and (positive? d) (positive? n)))
        (cons (/ (abs n) g) (/ (abs d) g))
        (cons (/ (neg (abs n)) g) (/ (abs d) g)))))

; ex 2.2
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (make-segment point1 point2)
  (cons point1 point2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; ex 2.3
(define (make-rect side1 side2)
  (cons side1 side2))
(define (side-1 rect)
  (car rect))
(define (side-2 rect)
  (cdr rect))

(define (square x)
  (* x x))

(define (perimeter rect)
  (+ (length (side-1 rect)) (length (side-2 rect))))

(define (area rect)
  (* (length (side-1 rect)) (length (side-2 rect))))

(define (length segment)
  (sqrt (+ (square x-length segment) (square y-length segment))))

(define (x-length segment)
  (- (x-point (end-segment segment)) (x-point (start-segment segment))))

(define (y-length segment)
  (- (y-point (end-segment segment)) (y-point (start-segment segment))))

; ex 2.4
(define (cons-l x y)
  ; returns a lambda that applies a function to (x y)
  (lambda (m) (m x y)))

(define (car-l z)
  ; a function that selects the first argument
  (z (lambda (p q) p)))

(define (cdr-l z)
  ; a function that selects the second argument
  (z (lambda (p q) q)))

; ex 2.5
(define (cons-i a b)
  (* (expt 2 a) (expt 3 b)))
; ...try later...

; ex 2.6
; Church numerals

(define zero-c (lambda (f) (lambda (x) x)))

(define (add1-c n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one-c (lambda (f) (lambda (x) (f x))))

(define two-c (lambda (f) (lambda (x) (f (f x)))))

(define (add-c a b)
  (lambda (f) (lambda (x) ((a f) ((b f)) x))))

; ex 2.7
; Interval arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (lower-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (divide-interval x y)
  (if (= 0 y)
      (error "Cannot divide by zero")
      (mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))))

(define (make-interval lower upper) (cons lower upper))

(define (upper-bound interval)
  (cdr interval))
(define (lower-bound interval)
  (car interval))

; ex 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y)
                    (upper-bound x) (lower-bound y))))

; ex 2.9
; too esoteric

; ex 2.10
; see 2.7

; ex 2.11
; Ben Bitdiddle should stick to diddling his own bits. Implementing this idea would obfuscate the code with very little performance gain.

; ex 2.12

(define (make-center-percent center prct-tolerance)
  (let ((radius (/ (* center (/ prct-tolerance 100.0)) 2)))
    (make-interval (- center radius) (+ center radius))))

(define (percent interval)
  ; assume that upper-bound and lower-bound are equidistant from the center
  (let ((center (/ (+ (upper-bound interval) (lower-bound interval)) 2))
        (radius (/ (- (upper-bound interval) (lower-bound interval)) 2)))
    (* (/ radius center) 100.0)))
  
; ex 2.13
; Book work
  
; ex 2.14-2.17
; Too esoteric

; ex 2.17
(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

; ex 2.18
(define (reverse l)
  (define (iter l result)
    (if (null? l)
        result
        (iter (cdr l) (cons (car l) result))))
  (iter l '()))

; ex 2.19
;(define us-coins (list 50 25 10 5 1))
;(define uk-coins (list 100 50 20 10 5 2 1))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

; The order of the list of coin values affects output. The coin values should be ordered from greatest at the left to smallest at the right.

; ex 2.20
(define (same-parity . args)
  (define (parity x)
    (remainder x 2))
  (define (recur first-parity l)
    (cond ((null? l) '())
          ((= (parity (car l)) first-parity) (cons (car l) (recur first-parity (cdr l))))
          (else (recur first-parity (cdr l)))))
  (cons (car args) (recur (parity (car args)) (cdr args))))

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list-map items factor)
  (map (lambda (x) (* factor x)) items))

; ex 2.21
(define (square-list l)
  (if (null? l)
      '()
      (cons (square (car l)) (square-list (cdr l)))))
(define (square-list-map l)
  (map (lambda (x) (square x)) l))

; ex 2.22
; The correct version is as follows:
(define (square-list-iter l)
  (define (iter l result)
    (if (null? l)
        result
        (iter (cdr l) (append result (list (square (car l)))))))
  (iter l '()))

; ex 2.23
(define (for-each proc l)
  (cond ((null? l) #t)
        (else (proc (car l))
              (for-each proc (cdr l)))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))

; ex 2.24
; book work

; ex 2.25
(define (ex-25)
  (let ((l1 (list 1 3 (list 5 7) 9))
        (l2 (list (list 7)))
        (l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))
    (= (car (cdr (car (cdr (cdr l1)))))
       (caar l2)
       (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))
       7)))

; ex 2.26
; note: append is basically concat
(define (ex-26)
  (let ((x (list 1 2 3))
        (y (list 4 5 6)))
    (and (equal? (append x y) (list 1 2 3 4 5 6))
         (equal? (cons x y) (list (list 1 2 3) 4 5 6))
         (equal? (list x y) (list (list 1 2 3) (list 4 5 6))))))

; ex 2.27
(define (deep-reverse l)
  (define (recur l result)
    (cond ((null? l) result)
          ((pair? (car l)) (recur (cdr l) (cons (recur (car l) '()) result)))
          (else (recur (cdr l) (cons (car l) result)))))
  (recur l '()))

(define (ex-27)
  (let ((x (list (list 1 2) (list 3 4))))
    (deep-reverse x)))

; ex 2.28
(define (fringe l)
  ; flattens an abitrarily nested list
  (cond
    ((null? l) '())
    ((pair? l) (append (fringe (car l)) (fringe (cdr l))))
    (else (list l))))

(define (ex-2.28)
  (let ((x (list (list 1 2) (list 3 4))))
    (fringe x)))

; ex 2.29

;(define (make-mobile left right)
;  (list left right))

;(define (make-branch length structure)
;  (list length structure))

;a
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))

;b

(define (mobile? s)
  (pair? s))

(define (weight? s)
  (and (not (mobile? s))
       (number? s)))

(define (mobile-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (cond ((weight? structure) structure)
          (mobile-weight branch))))

;c

(define (balanced? mobile)
  (= (* (branch-weight (left-branch mobile)) (branch-length (left-branch mobile)))
     (* (branch-weight (right-branch mobile)) (branch-length (right-branch mobile)))))

(define (ex-2.29)
  (define balanced-r-branch (make-branch 4 100))
  (define unbalanced-r-branch (make-branch 4 10))
  (define balanced-l-branch (make-branch 8 50))
  (define 2-balanced (make-mobile balanced-l-branch balanced-r-branch))
  (define 2-unbalanced (make-mobile balanced-l-branch unbalanced-r-branch))
  (display (mobile-weight 2-balanced))
  (newline)
  (display (mobile-weight 2-unbalanced))
  (newline)
  (display (balanced? 2-balanced))
  (newline)
  (display (balanced? 2-unbalanced)))

; d
; Change the representation

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))