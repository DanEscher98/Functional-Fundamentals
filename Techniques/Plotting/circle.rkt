#lang at-exp racket

(require infix plot)

(define r 20)
(define R (* r 10))

; The Ciclotoid
(define (ciclotoid t) @${ vector[r*(t - sin[t]), r*(1 - cos[t])] })

(plot (list (axes)
			(parametric ciclotoid 0 (* 2 pi) #:color "blue"))
	  #:x-min 0 #:x-max (* r 2 pi)
	  #:y-min 0 #:y-max (* r 2 pi)
	  #:title "Ciclotoid"
	  #:out-file "ciclotoid.svg")

; The Hipocycloid
(define (circle t) @${ vector[R*cos[t], R*sin[t]] })
(define (hypocycloid phi)
	@${ vector[
			   (R-r)*cos[phi] + r*cos[(R-r)/r * phi],
			   (R-r)*sin[phi] - r*sin[(R-r)/r * phi]] })

(plot (list	(axes)
			(parametric circle 0 (* r 2 pi) #:color "black" #:width 2)
			(parametric hypocycloid 0 (* r 2 pi) #:color "blue" #:widht 1))
	  #:x-min (- -10 R) #:x-max (+ 10 R)
	  #:y-min (- -10 R) #:y-max (+ 10 R)
	  #:title "Hypocycloid"
	  #:out-file "hypocycloid.svg")
