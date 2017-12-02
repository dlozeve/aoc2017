#!/usr/bin/csi -script

(use extras)

(define (range xs)
  (- (apply max xs) (apply min xs)))

(define (find-divisors x xs)
  (cond ((null? xs) 0)
	((= 0 (remainder x (car xs))) (/ x (car xs)))
	((= 0 (remainder (car xs) x)) (/ (car xs) x))
	(else (find-divisors x (cdr xs)))))

(define (divisors xs)
  (if (null? xs) 0 
      (let* ((y (car xs))
	     (ys (cdr xs))
	     (first-divisor (find-divisors y ys)))
	(if (= 0 first-divisor) (divisors ys) first-divisor))))

(define (main)
  (let* ((contents (read-lines))
	 (numbers (map (lambda (x) (map string->number x)) (map string-split contents))))
    (print (apply + (map range numbers)))
    (print (apply + (map divisors numbers)))))

(main)
