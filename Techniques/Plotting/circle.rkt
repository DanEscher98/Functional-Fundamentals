#lang at-exp racket

(require infix plot)

(define r 30)
(define off (+ 5 (* 2 r)))

; algebraic half-circle
(define (c1 x) @${ sqrt[r^2 - x^2]})
(define (c2 x) @${-sqrt[r^2 - x^2]})

; parametric circle
(define (cp t) @${ vector[off + r*cos[t], r*sin[t]] })


(plot (list (axes)
			(function c1 (- r) r #:color "blue" #:label "c1")
			(function c2 (- r) r #:color "red"  #:label "c2")
			(parametric cp 0 (* 2 pi) #:color "green" #:label "cp" #:width 2))
	  #:x-min (- r) #:x-max (+ off r)
	  #:y-min (- r) #:y-max (+ off r)
	  #:legend-anchor 'top-right
	  #:out-file "circle.svg")


; The Ciclotoid
(define (ciclotoid t) @${ vector[r*(t - sin[t]), r*(1 - cos[t])] }) 
