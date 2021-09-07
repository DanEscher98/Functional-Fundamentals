#lang at-exp racket
(require infix plot)

(plot (list
		(polar (lambda (theta) (* 2 pi)) 0 (* 2 pi))
		(polar (lambda (theta) theta)	 0 (* 2 pi)))
	  #:x-min -10 #:x-max 10
	  #:y-min -10 #:y-max 10
	  #:out-file "polar-circle.svg")

; Polar roses
(define (rose k t) @${sin[k*t]})

(parameterize
	([plot-width 200]
	 [plot-height 200]
	 [plot-tick-size 0]
	 [plot-font-size 0]
	 [plot-x-label #f]
	 [plot-y-label #f])

	(for/list ([k (in-range 1 5 1)])
		(plot (list
				(polar (lambda (t) (rose k t)) 0 (* 4 pi)))
			  #:x-min -1 #:x-max 1
			  #:y-min -1 #:y-max 1
			  #:out-file (string-append "Output/rose-"
										(number->string k)
										".svg"))))
