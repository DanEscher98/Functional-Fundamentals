#lang racket/gui
(require math/matrix)

(define selected-row -1)
(define selected-col -1)
(define show-card #t)

(define card-names
	#("101" "102" "103" "104" "105" "106" "107" "108" "109" "110" "111" "112" "113"
	  "201" "202" "203" "204" "205" "206" "207" "208" "209" "210" "211" "212" "213"
	  "301" "302" "303" "304" "305" "306" "307" "308" "309" "310" "311" "312" "313"
	  "401" "402" "403" "404" "405" "406" "407" "408" "409" "410" "411" "412" "413"))

(define card-deck
	(for/vector ([card-name (in-vector card-names)])
		(read-bitmap (build-path "Cards" (string-append card-name ".svg")))))

(define card-width (send (vector-ref card-deck 0) get-width))
(define card-height (send (vector-ref card-deck 0) get-height))

; Revuelve el mazo de cartas
(define (shuffle-deck)
	(for ([i (in-range 52)])
		(let ([j (random 52)]
			  [t (vector-ref card-deck i)])
			(vector-set! card-deck i (vector-ref card-deck j))
			(vector-set! card-deck j t))))

(define SIZE 5)
(define display-matrix
	(build-matrix SIZE SIZE (lambda (r c) (+ (* r SIZE) c))))

(define (get-card r c)
	(vector-ref card-deck (matrix-ref display-matrix r c)))

(define main-frame
	(new frame%
		 [label "Pick a Card"]
		 [width 550] 
		 [height 650]))

(define main-panel
	(new horizontal-panel%
		[parent main-frame]))

(define control-panel
	(new vertical-panel%
		 [parent main-panel]
		 [min-width 100]
		 [stretchable-width 100]))

(define MARGIN 10) ; in pixels
(define MSG-HEIGHT 20) ; height of msg label
(define canvas
	(new canvas%
		 [parent main-frame]
		 [min-width 400]
		 [paint-callback
			(lambda (canvas dc)
				(send dc set-smoothing 'smoothed)
				(for* ([r (in-range SIZE)]	; draw cards
					   [c (in-range SIZE)])
					(send dc draw-bitmap (get-card r c)
						  (+ MARGIN (* c (+ MARGIN card-width)))
						  (+ MSG-HEIGHT (* r (+ MARGIN card-height)))))
				(when show-card				; draw red border on selected card
					(let* ([off-x (/ MARGIN 2)]
						   [off-y (+ off-x MSG-HEIGHT)])
						(send dc set-pen "red" 3 'solid)
						(send dc set-brush (new brush% [style 'transparent]))
						(send dc draw-rectangle
							  (+ off-x (* selected-col (+ MARGIN card-width)))
							  (+ off-y (* selected-row (+ MARGIN card-height)))
							  (+ card-width MARGIN)
							  (+ card-height MARGIN))
						(send dc set-pen "black" 2 'solid))))]))

(define msg
	(new message%
		 [parent control-panel]
		 [min-height MSG-HEIGHT]
		 [label "Select again."]))

(define arrow
	(let* ([image (make-bitmap 50 40)]
		   [dc (new bitmap-dc% [bitmap image])]
		   [path (new dc-path%)])
		(send dc set-brush (new brush% [color "blue"]))
		(send path move-to 0 10)
		(send path lines '((30 . 10)
						   (30 . 0)
						   (50 . 20)
						   (30 . 40)
						   (30 . 30)
						   (0 . 30)
						   (0 . 10)))
		(send dc draw-path path)
		image))

(define (make-swap r1 r2)
	(define (swap-func r c)
		(cond [(= r r1) (if (= c r2) 1 0)]
			  [(= r r2) (if (= c r1) 1 0)]
			  [(= r c) 1]
			  [else 0]))
	(build-matrix SIZE SIZE swap-func))

(define (swap-cols c1 c2)
	(let ([swap-matrix (make-swap c1 c2)])
		(matrix* display-matrix swap-matrix)))

(define (show-selection r)
	(send msg set-label "Tada!")
	(set! selected-row r)
	(set! show-card #t)
	(send canvas refresh-now))

(define (first-row-selection r)
	(set! selected-col r)
	(send msg set-label "Select again.")
	(for ([i (in-range SIZE)])
		(let ([j (random SIZE)]
			  [t (vector-ref card-deck i)])
			(set! display-matrix (swap-cols i j))))
	(set! display-matrix (matrix-transpose display-matrix))
	(send canvas refresh-now))

(define (select-row r)
	(cond [(< selected-col 0) (first-row-selection r)]
		  [(< selected-row 0) (show-selection r)]
		  [else (send msg set-label "Restart.")]))

(define (gen-row-button r)
	(new button%
		 [parent control-panel]
		 [label arrow]
		 [min-width 80]
		 [min-height 50]
		 [vert-margin (/ (+ MARGIN (- card-height 50)) 2)]
		 [callback (lambda (button event)
						(select-row r))]))

(for ([i (in-range SIZE)])
	(gen-row-button i))

(define (restart)
	(shuffle-deck)
	(send msg set-label "Select a row.")
	(set! show-card #f)
	(set! selected-row -1)
	(set! selected-col -1)
	(send canvas refresh-now))

(restart)
(send main-frame show #t)
