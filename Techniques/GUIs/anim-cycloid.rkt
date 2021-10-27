#lang at-exp racket/gui
(require infix plot)

; Global variables
(define r 30)
(define angle 0)

(define (cycloid t) @${ vector[r*(t - sin[t]), r*(1 - cos[t])] })
(define (circle t) @${ vector[r*angle + r*sin[t], r + r*cos[t]] })

(define (line-points t)
	(let ([x @${r*(t - sin[t])}]
		  [y @${r*(1 - cos[t])}]
		  [x0 (* r angle)]
		  [y0 r])
		(list
			(lines (list (vector x0 y0) (vector x y))
				#:width 2
				#:color "blue")
			(points (list (vector x0 y0) (vector x y))
				#:size 10
				#:color "red"
				#:sym 'fullcircle1
				#:fill-color "red"))))

; Plot section
(plot-decorations? #f)
(define (cycloid-plot dc)
	(plot/dc (append (list (axes)
						(parametric circle 0 (* 2 pi) #:color "green")
						(parametric cycloid 0 angle #:color "red"))
					(line-points angle))
			 dc 10 25 320 150
			 #:x-min 0 #:x-max (+ (* r 2 pi) r)
			 #:y-min 0 #:y-max (* r pi)))

(define main-frame
	(new frame%
		 [label "The Cycloid"]
		 [width 350]
		 [height 200]))

(define canvas
	(new canvas% [parent main-frame]
		 [paint-callback
			(lambda (canvas dc)
				(send dc set-smoothing 'smoothed)
				(send dc draw-text "The Cycloid" 10 180)
				(cycloid-plot dc))]))

(define timer
	(new timer% [notify-callback
				  (lambda ()
						(set! angle (+ 0.1 angle))
						(when (> angle (* 2 pi)) (set! angle 0))
						(send canvas refresh-now))]))

(send main-frame show #t)
(send timer start 10)
