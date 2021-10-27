#lang racket
#|
	the "for" family
	- for
	- for/list
	- for/and, for/or
	- for/sum, for/product
	- for/first, for/last
|#

(define (sigmaR n)
	(if (zero? n) 0
		(+ n (sigmaR (sub1 n)))))

; Loop Lisp-like
(define (sigmaL n)
	(let loop ([n n] [acc 0])
		(if (zero? n) acc
			(loop (sub1 n) (+ acc n)))))

; Loop Python-like
(define (sigmaLP n)
	(let ([s 0])				; initialize s to zero
		(do ()					; an optional initializer statement can go here
			((< n 1))			; do until this becomes true
			(set! s (+ s n))
			(set! n (- n 1)))
		s))

; Loop C-like
(define (sigmaLC n)
	(let ([s 0])
		(do ([i 1 (add1 i)])	; initialize i=1, set i = i+1 at each iteration
			((> i n) s)			; do until i>n, then return s
			(set! s (+ s i)))))

(define (list-chars str)
	(for ([c str] [i (in-naturals)])
		(printf "~a: ~a\n" i c)))

(let* ([h "Hello"] [l (string-length h)])
	(for ([i (in-range l)])
		(display (string-ref h i))
		(newline)))

; The squares of first even numbers less than 10
(define fstEvenSqr 
	(for/list ([x (in-naturals)] #:break (> x 10) #:when (even? x))
		(sqr x)))

(for* ([i (in-range 1 5 2)] [j (in-range 1 2)])
	(display (list i j (* i j)))
	(newline))

(for/and ([x '(2 4 6 8)]) (even? x))

; closures
(define (make-comp bal int)
	(let ([rate (add1 (/ int 100.0))])
		(lambda ()
			(set! bal (* bal rate))
			(round bal))))

(define (fact)
	(let ([h (make-hash)]) ; hash table to contain memoized values
		(define (loop n) (cond
			((hash-has-key? h n) (hash-ref h n))
			(else (let
				([f (* n (loop (sub1 n)))])
				(hash-set! h n f)
				f))))
		loop))

