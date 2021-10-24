(import (scheme base)
		(scheme write))

;; Print Resul
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

;; Sum Biggest Squares
;;
;; Takes 3 arguments and returns the sum of squares of the two largest
(define (sum-big-squares a b c)
  (define (sq n) (* n n))
  (define (sum-of-squares a b)
	(+ (sq a) (sq b)))
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
