#lang racket

(require plot plot/no-gui)

(plot (function sin #:color "Blue")
      #:x-min (* -2 pi) #:x-max (* 2 pi)
      #:title "Sine function"
	  #:out-file "function-sine.png")

(plot-file (list (axes)
			(function sin #:color "Blue" #:label "sin" #:style 'dot)
			(function cos 0 (* 2 pi) #:color "red" #:label "cos"))
		#:x-min (* -2 pi) #:x-max (* 2 pi)
		#:y-min -2 #:y-max 2
		#:title "Sine and Cosine"
		#:x-label "X"
		#:y-label #f
		#:out-file "Sine and Cosine"
		#:'png)

(plot (list (axes)
			(function sin #:color "Blue" #:label "sin" #:style 'dot)
			(inverse sqr -2 2 #:color "red" #:label "x²" #:width 2))
		#:x-min (* -1 pi) #:x-max pi
		#:y-min -3 #:y-max 3)
