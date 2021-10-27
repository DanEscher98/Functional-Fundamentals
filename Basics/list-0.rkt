#lang racket

#|
funciones usadas:
	define, lambda
	car, cdr
	pair?, null?, eq? 
	cond, and, or, not

functiones implementadas: 
	atom?, atomList?, member?
	remove
|#


(define (one? a) (= 0 (sub1 a)))

; Verifica que el valor de entrada sea un átomo
(define atom?
	(lambda (x)
		(and (not (pair? x)) (not (null? x)))))

; Verifica que la lista sólo contenga átomos
(define atomList?
	(lambda (l)
		(cond
			((null? l) #t)
			((atom? (car l)) (atomList? (cdr l)))
			(else #f))))

; Verifica que el valor se encuentre en la lista
(define member?
	(lambda (a lat)
		(cond
			((null? lat) #f)
			(else (or (eq? a (car lat))
					  (member? a (cdr lat)))))))

; Elimina la primera aparición del valor en la lista
(define remFst
	(lambda (a lat)
		(cond
		  	((null? lat) '())
			((equal? a (car lat)) (cdr lat))
			(else (cons
				(car lat) (remFst a (cdr lat)))))))

; Retorna el n elemento de una lista
(define pick
	(lambda (n lat)
		(cond
			((null? lat) '())
			((zero? n) (car lat))
			(else (pick (sub1 n) (cdr lat))))))

; Remueve el n elemento de una lista
(define rempick
	(lambda (n lat)
		(cond
			((null? lat) '())
			((zero? n) (cdr lat))
			(else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

; Retorna el número de veces que 'a' se encuentra en lat
(define occur
	(lambda (a lat)
		(cond
			((null? lat) 0)
			((equal? a (car lat)) (add1 (occur a (cdr lat))))
			(else (occur a (cdr lat))))))

