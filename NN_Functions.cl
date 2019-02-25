;; ------------------ Initilization ------------------
;; Inputs
(setf inputs (make-array '(3 2) :initial-contents '( (1 2) (-3 5) (7 12) )) )

;; Weights
(setf weights (make-array '(2 4) :initial-contents '( (2 -4 5 8) (-1 -3.7 1.5 2) )) )
;; ------------------ End Initilization ------------------

;; ------------------ Utility Functions ------------------
;; Multiply 2 matricies together
;; Matrix a = m * s (dimensions)
;; Matrix b = s * n	(dimensions)
(defun mult (a b)
	(let* 	(
				(m (nth 0 (array-dimensions a)) )
				(s (nth 1 (array-dimensions a)) )
				(n (nth 1 (array-dimensions b)) )
				(result (make-array (list m n))	)
			)
	
	(dotimes (i m)
		(dotimes (j n)
			(setf (aref result i j) 0.0)
			(dotimes (k s)
				(incf (aref result i j) (* (aref a i k) (aref b k j)))
			)
		)
	)
	result
	)
)

;; Pads NN Input Matrix with 1's
;; Resulting Matrix = m * (n + 1) [Dummy values needs extra column]
(defun prepend1 (a)
	
	(let* 	(
				(m (nth 0 (array-dimensions a)) 		)
				(n (nth 1 (array-dimensions a))			)
				(result (make-array (list m (+ n 1)))	)
			)
	
	;; Iterate rows
	(dotimes (i m)
		;; i(th) row in column 0
		(setf (aref result i 0) 1.0)
	)
	
	;; Iterate rows
	(dotimes (i m)
		;; Iterate columns
		(dotimes (j n)
			;; Set matrix values infront of padded 1's in column 0
			(setf (aref result i (+ 1 j)) (aref a i j))
		)
	)
	result
	)
	
)

;; Applies heavyside function to a single element
(defun heavyside (x)
	"Neural Net heavy-side function"
	(cond	((< x 0) 0 )
			(t		 1 )
	)
)

;; Applies heavyside function element wise (matrix as input)
(defun elheavyside (a)
	"Apply heavy-side function to each element of a matrix"
	(let*	(	(m			(nth 0 		(array-dimensions a)) 	)
				(n			(nth 1 		(array-dimensions a)) 	)
				(result		(make-array (list m n)) 	 		)
			)
	
		(dotimes (i m)
			(dotimes (j n)
				(setf (aref result i j) (heavyside (aref a i j)) )
			)
		)
		result
	)
)

;; Fitness Functions
;; (defun (X A d))
;; ------------------ End Utility Functions ------------------

;; ------------------ Neural Network ------------------
;;	Xbar-			Xbar			A			Xbar*A			o=f(Xbar*A)
;;	(x1 x2)			(1 x1 x2)		(. . .)		(. . .)			f(. . .)
;;									(. . .)
;;									(. . .)
;; (Inputs + 1) * (Outputs) = Size of A (Weight Matrix)
;; Single-Layered Neural Network (Feed-Forward)
(defun NN1 (x A)
	(let*	(	(m			(nth 0 		(array-dimensions x)) 	)
				(n			(nth 1 		(array-dimensions A)) 	)
				(result		(make-array (list m n)) 			)
			)
			
	(setf result (elheavyside (mult (prepend1 x) A))			)
	)
)
;; ------------------ End Neural Network ------------------