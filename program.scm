(import (scheme base)
		(scheme write))

;; Print Result
;;
;; Displays the result of a given expression, followed by a newline
(define (p e)
  (display e)(newline)
  e)

;;; Exercise 1.1

(p (+ 5 3 4))
(p (- 9 1))
(p (/ 6 2))
(p (+ (* 2 4) (- 4 6)))
(define a 3)
(define b (+ a 1))
(p (+ a b (* a b)))
(p (= a b))
(p (if (and (> b a) (< b (* a b)))
	   b
	   a))
(p (cond ((= a 4) 6)
		 ((= b 4) (+ 6 7 a))
		 (else 25)))
(p (+ 2 (if (> b a) b a)))
(p (* (cond ((> a b) a)
			((< a b) b)
			(else -1))
	  (+ a 1)))

;;; Exercise 1.2
(p (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
	  (* 3 (- 6 2) (- 2 7))))

;;; Exercise 1.3

(define (square n) (* n n))

;; Sum Biggest Squares
;;
;; Takes 3 arguments and returns the sum of squares of the two largest
(define (sum-big-squares a b c)
  (define (sum-of-squares a b)
	(+ (square a) (square b)))
  (define (max n m)
	(if (> n m) n m))
  (if (> a b)
	  (sum-of-squares a (max b c))
	  (sum-of-squares b (max a c))))

(p (sum-big-squares 3 2 10))

;;; Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(p (a-plus-abs-b 100 5))
(p (a-plus-abs-b 3 -7))

;;; Exercise 1.5

(define (t) (t))
(define (test x y)
  (if (= x 0)
	  0
	  y))
; (p (test 0 (t))) ; ~> would recurse indefinitely because earger applicative order
(define-syntax testm
  (syntax-rules ()
	((_ x y)
	 (if (= x 0)
		 0
		 y))))
(p (testm 0 (t))) ; -> Fine, because macro is transformed before evaluation


;;; Exercise 1.6

(define (abs n)
  (if (< n 0)
	  (- n)
	  n))
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt x)
  (define (improve guess)
	(average guess (/ x guess)))
  (define (good-enough? guess)
	(< (abs (- (square guess) x)) 0.001))
  (define (sqrt-iter guess)
	(if (good-enough? guess)
		guess
		(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(p (sqrt 9))
(p (sqrt (+ 100 37)))
(p (sqrt (+ (sqrt 2) (sqrt 3))))
(p (square (sqrt 1000)))

;; new-if, a procedure rather than syntax, will eagerly evaluate both clauses!
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
		(else else-clause)))
(p (new-if (= 2 3 ) 0 5))
(p (new-if (= 1 1 ) 0 5))
(new-if (= 2 3) (p "oh noes!") (p "yep"))

;;; Exercise 1.9

(let ((inc (lambda (x) (+ x 1)))
	  (dec (lambda (y) (- y 1))))

  ;; Recursive addition. This will explode the stack
  (define (+ a b)
	(if (= a 0)
		b
		(inc (+ (dec a) b))))
  (p (+ 4 5))

  ;; Tail recursive version. This uses contant stack space
  (define (+ a b)
	(if (= a 0)
		b
		(+ (dec a) (inc b))))
  (p (+ 4 5)))

;;;; Exercise 1.10

;; The Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
		((= x 0) (* 2 y))
		((= y 1) 2)
		(else (A (- x 1)
				 (A x (- y 1))))))
(p (A 1 10))
(p (A 2 4))
(p (A 3 3))

; exit code..
0
