;; $ csc day17.scm -o day17 -O5
;; $ ./day17 -:hm8g

(use srfi-1)
(declare (fixnum))

(define (prepend! x list)
  (set-cdr! list (cons (car list) (cdr list)))
  (set-car! list x))

(define (spin steps n after)
  (let ((buffer (circular-list 0)))
    (do ((i 1 (+ i 1)))
	((> i n) (cadr (member after buffer)))
      (let ((new (drop buffer (+ steps 1))))
	(prepend! i new)
	(set! buffer new)))))

(write (spin 3 2017 2017))
(newline)
(write (spin 312 2017 2017))
(newline)
(write (spin 312 50000000 0))
(newline)

