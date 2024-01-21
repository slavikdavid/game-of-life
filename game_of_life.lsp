(defvar *grid-x-size* 32)
(defvar *grid-y-size* 4)

(setf *random-state* (make-random-state t))

(defun make-2d-array (rows cols &optional initial-value)
  (let ((array (make-array (list rows cols) :initial-element initial-value)))
    array))

(defun set-value (array row col value)
  (setf (aref array row col) value))

(defun get-value (array row col)
  (aref array row col))

(defun randomize-array (array)
  (let ((num-rows (array-dimension array 0))
        (num-cols (array-dimension array 1)))
    (loop for row from 0 below num-rows do
      (loop for col from 0 below num-cols do
        (set-value array row col (random 2))))))

(defun print-2d-array (array)
  (let ((num-rows (array-dimension array 0))
        (num-cols (array-dimension array 1)))
    (loop for row from 0 below num-rows do
      (loop for col from 0 below num-cols do
        (format t "~4d " (get-value array row col))
        (if (= col (- num-cols 1))
            (format t "~%")))))
    (format t "~%"))

(defun draw-2d-array (array)
  (let ((num-rows (array-dimension array 0))
        (num-cols (array-dimension array 1)))
    (format t "\033[2J") 
    (format t "\033[H")
    (loop for row from 0 below num-rows do
      (loop for col from 0 below num-cols do
        (if (= (aref array row col) 1)
            (format t "█")
            (format t " "))
        (if (= col (- num-cols 1))
            (format t "~%")
        )
      )
    )
    (format t "~%")
  ))

(defun count-surrounding-cells (grid x y)
  (let ((height (array-dimension grid 0))
        (width (array-dimension grid 1))
        (count 0))
    (loop for dx from -1 to 1 do
      (loop for dy from -1 to 1 do
        (when (and (not (and (= dx 0) (= dy 0)))
                     (>= (+ x dx) 0) (< (+ x dx) width)
                     (>= (+ y dy) 0) (< (+ y dy) height))
          (incf count (aref grid (+ y dy) (+ x dx)))))
      )
    count))

(defun update-grid (grid)
  (defvar *height* (array-dimension grid 0))
  (defvar *width* (array-dimension grid 1))
  (let ((new-grid (make-array (list *height* *width*) :initial-element 0)))


    (loop for x from 0 below *width* do
      (loop for y from 0 below *height* do
        (setf (aref new-grid y x) (aref grid y x))))


    (loop for x from 0 below *width* do
      (loop for y from 0 below *height* do
        (let ((cell (aref new-grid y x))
              (neighbors (count-surrounding-cells grid x y)))
          (cond
            ((= cell 1)
             (setf (aref new-grid y x)
                   (if (or (< neighbors 2) (> neighbors 3))
                       0
                       1)))
            ((= cell 0)
             (setf (aref new-grid y x)
                   (if (= neighbors 3)
                       1
                       0)))))
        )
      )
    new-grid))


(defun run-game-of-life (generations)

  (setq my-2d-array (make-array (list *grid-y-size* *grid-x-size*) :initial-element 0))

  (randomize-array my-2d-array)

  (loop for gen from 1 to generations do
    (draw-2d-array my-2d-array)
    (setq my-2d-array (update-grid my-2d-array))
    (sleep 0.2)
  )
)

(run-game-of-life 150)
