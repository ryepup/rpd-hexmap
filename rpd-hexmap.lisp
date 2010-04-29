;;; -*- mode: lisp; indent-tabs: nil -*-

(in-package :rpd-hexmap)

(defun make-map (width height)
  (make-array (list (* 2 width) (* 2 height))))

(defvar *hex-surface* nil "sdl surface for a hexagon")
(defvar *unit-hex* '((0 8) (6 16) (16 16) (22 8) (16 0) (6 0) (0 8)))
(defun draw-map (map &key (hex-size-multiplier 1))
  (let ((hex-width (1+ (* hex-size-multiplier
			  (apply #'max (mapcar #'first *unit-hex*)))))
	(hex-height (1+ (* hex-size-multiplier
			   (apply #'max (mapcar #'second *unit-hex*))))))
    (setf *hex-surface* (sdl:create-surface hex-width hex-height))
    (sdl:draw-polygon (iter (for (x y) in *unit-hex*)
			    (collect (sdl:point :x (* hex-size-multiplier x)
						:y (* hex-size-multiplier y))))
		      :surface *hex-surface*
		      :color sdl:*white* )
    (iter (with y-tile-offset = (* hex-size-multiplier
				   (second (first *unit-hex*))))
	  (with x-tile-offset = (+ hex-width
				   (* hex-size-multiplier
				      (- (first (third *unit-hex*))
					 (first (second *unit-hex*))))))
	  (with x-odd-row-offset = (* hex-size-multiplier
				      (first (third *unit-hex*))))
	  (for y from 0 to (sdl:height sdl:*default-display*) by y-tile-offset)
	  (for rowy from 0)
	  (iter (for x from (if (evenp rowy) 0 x-odd-row-offset) to
		     (sdl:width sdl:*default-display*) by x-tile-offset)
		(sdl:draw-surface-at *hex-surface* (sdl:point :x x :y y))))))

(defmacro with-hexmap ((map width height) &body body)
  `(let ((,map (make-map ,width ,height)))
     (prog1
	 (progn ,@body)
       (when *hex-surface*
	 (sdl:free *hex-surface*)
	 (setf *hex-surface* nil)))))

(defvar *map* nil)
(defun test ()
  (sdl:with-init ()
    (sdl:window 400 400)
    (setf (sdl:frame-rate) 0)      
    (sdl:clear-display sdl:*black*)
    (with-hexmap (*map* 20 20)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:idle ()
	       (draw-map *map*)
	       (sdl:update-display))))))


