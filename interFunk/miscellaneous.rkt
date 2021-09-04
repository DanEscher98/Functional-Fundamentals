#lang racket


(define (hanoi n a b)
	(if (= n 1)
		(printf "~a -> ~a\n" a b)
		(let ([u (- 3 (+ a b))])
			(hanoi (sub1 n) a u)
			(printf "~a -> ~a\n" a b)
			(hanoi (sub1 n) u b))))

CE6B-D865
