#lang at-exp racket
(require infix plot racket/draw)

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

; Fibonacci spiral
(define WIDTH 600)
(define HEIGTH 400)
(define UNIT 6)		; pixels in unit-width square
(define OFFSET-X 140)
(define OFFSET-Y 75)

