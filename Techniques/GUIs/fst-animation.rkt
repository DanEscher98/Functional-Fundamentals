#lang racket/gui

(define RADIUS 8)
(define DIAMETER (* 2 RADIUS))

(define loc-x RADIUS)
(define loc-y 35)

(define main-frame
	(new frame%
		 [label "I'm a GUI"]
		 [width 300]
		 [height 100]))

(define canvas
	(new canvas% [parent main-frame]
		 [paint-callback
			(lambda (canvas dc)
				(send dc set-smoothing 'smoothed)
				(send dc set-text-foreground "blue")
				(send dc draw-text "A bouncing ball" 0 0)
				(send dc set-brush "red" 'solid)
				(send dc draw-ellipse (- loc-x RADIUS) (- loc-y RADIUS)
					  DIAMETER DIAMETER))]))

(define timer
	(let ([delta 2])
		(new timer%
			 [notify-callback
				(lambda ()
					(cond [(<= loc-x RADIUS) (set! delta 2)]
						  [(>= loc-x (- (send canvas get-width) RADIUS))
						   (set! delta -2)])
					(set! loc-x (+ loc-x delta))
					(send canvas refresh-now))])))

(send main-frame show #t)
(send timer start 50)
