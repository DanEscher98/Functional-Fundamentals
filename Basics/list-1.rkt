#lang racket

#|
funciones implementadas:
	init, last, drop, take, reverse
|#

; Devuelve los primeros elementos de una lista
(define iniciales
	(lambda (lat) (cond
		((null? (cdr (cdr lat))) (car lat))
		(else (cons (car lat) (iniciales (cdr lat)))))))

; Devuelve el último elemento de una lista
(define ultimo
	(lambda (lat) (cond
		((null? (cdr (cdr lat)) (car (cdr lat))))
		(else (iniciales (car lat))))))

; Elimina los primeros n elementos de una lista
(define tira
	(lambda (n lat) (cond
		((null? lat) '())
		((zero? n) lat)
		(else (tira (sub1 n) (cdr lat))))))

; Devuelve los primeros n elementos de una lista
(define toma
	(lambda (n lat) (cond
		((null? lat) '())
		((zero? n) '())
		(else (cons (car lat) (tira (sub1 n) (cdr lat)))))))

; Concatena dos listas
(define concatena
	(lambda (a_lat b_lat) (cond
		((null? a_lat) b_lat)
		(else (concatena (iniciales a_lat)
						 (cons (ultimo a_lat) b_lat))))))

; Devuelve un par
(define (par a b) (cons a (cons b '())))

; Añade un elemento al final de una lista
(define (apendice a lat)
	(reverse (cons a (reverse lat))))

; Devuelve la lista en orden invertido
(define reversa
	(lambda (lat) (cond
		((null? lat) '())
		(else (cons (ultimo lat) (reversa (iniciales lat)))))))

; Devuelve un par, los primeros n elementos de una lista y el resto
(define (separaEn n lat)
	(define (auxSplit n p) (cond
		((zero? n) p)
		((null? (second p)) p)
		(else (separaEn (sub1 n)
						(par (apendice (car (second p)) (first p))
							 (cdr (second p)))))))
	(auxSplit n (par '() lat)))


