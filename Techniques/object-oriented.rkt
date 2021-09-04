#lang racket

(define queue%					; Racket class names end with %
	(class object%				; All classes must inherit from some parent class
		(init [queue-list '()])	; Initialization parameters for the class
		(define head '{})		; Our class uses head and tail pointer identifiers
		(define tail '{})
		(super-new)				; A required call to the super class (in this case object%)

		(define/public (enqueue val)		; Public methods are accessible from outside the class
			(let ([t (mcons val '())])		; mcons creates a mutable cons cell
				(if (null? head)
					(begin
						(set! head t)
						(set! tail t))
					(begin
						(set-mcdr! tail t)
						(set! tail t)))))

		(define/public (dequeue)			; Returns the value at the head of the queue
			(if (null? head)
				(error "Queue is empty")
				(let ([val (mcar head)])
					(set! head (mcdr head))	; The head pointer is updated to point to the next value
					(when (null? head) (set! tail '()))
					val)))

		(define/public (print-queue)
			(define (prt rest)
				(if (null? rest)
					(newline)
					(let ([h (mcar rest)] [t (mcdr rest)])
						(printf "~a " h)
						(prt t))))
			(prt head))
		
		(for ([v queue-list]) (enqueue v))))
