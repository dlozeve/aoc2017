#!/usr/bin/csi -script

(use extras)
(use srfi-1)
(use list-comprehensions)

(define (sum-matching-offset k xs)
  (let* ((n (length xs))
	 (newxs (zip xs (take (drop (concatenate ((repeat 3) xs)) k) n)))
	 (f (lambda (x acc) (if (= (first x) (second x)) (+ acc (first x)) acc))))
    (foldr f 0 newxs)))

(define (main)
  (let* ((digits-str (read-line))
	 (digits (map string->number (map string (string->list digits-str)))))
    (print (sum-matching-offset 1  digits))
    (print (sum-matching-offset (/ (length digits) 2) digits))))

(main)
