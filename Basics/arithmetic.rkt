#lang racket
; (enter! "file")

; built in
#|	
	define, lambda, cond, else, cons
	inc, dec, zero?, null?, eq?, null?
|#

(define (one? x) (zero? (sub1 x)))

(define suma
	(lambda (a b) (cond
		((zero? b) a)
		(else (add1 (suma a (sub1 b)))))))

(define resta
	(lambda (a b)(cond
		((zero? b) a)
		(else (sub1 (resta a (sub1 b)))))))

(define producto
	(lambda (a b) (cond
		((one? b) a)
		(else (suma a (producto a (sub1 b)))))))

(define greater? 
	(lambda (a b) (cond
		((zero? a) #f)
		((zero? b) #t)
		(else (greater? (sub1 a) (sub1 b))))))

(define smaller?
	(lambda (a b) (cond
		((zero? a) #t)
		((zero? b) #f)
		(else (smaller? (sub1 a) (sub1 b))))))

(define cociente
	(lambda (a b) (cond
		((smaller? a b) 0)
		(else (add1 (cociente (resta a b) b))))))

(define (division a b)
	(define (auxDiv q a b) (cond
		((zero? b) '())
		((not (greater? a b)) (cons q a))
		(else (auxDiv (add1 q) (resta a b) b))))
	(auxDiv 0 a b))


