#lang at-exp racket
(require infix plot)

; Golden spiral
(define golden
	@${(1 + sqrt[5])/2})

(define (golden-spiral theta)
	@${golden ^ (theta * 2 / pi)})

(plot (polar golden-spiral 0 (* 4 pi))
	  #:x-min -20 #:x-max 50
	  #:y-min -40 #:y-max 30
	  #:title "The Golden Spiral"
	  #:x-label #f #:y-label #f
	  #:out-file "golden-spiral.svg")
