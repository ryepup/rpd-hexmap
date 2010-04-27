;;; -*- mode: lisp; indent-tabs: nil -*-

(in-package :rpd-hexmap)

(defun make-map (width height)
  (make-array (list (* 2 width) (* 2 height))))

(defvar *hex-surface* nil "sdl surface for the hexagon")
(defun draw-map (map &key (hex-size-multiplier 3))
  (let ((*hex-surface* (sdl:create-surface (* hex-size-multiplier 12)
					   (* hex-size-multiplier 8))))
    (sdl:draw-polygon (mapcar #'(lambda (x y) (sdl:point :x (* hex-size-multiplier x)
							 :y (* hex-size-multiplier y)))
			      '(0 4 8 12 8 4 0)
			      '(4 8 8 4 0 0 4))
		      :surface *hex-surface*
		      :color sdl:*white* )
    (iter (for x from 0 to (sdl:width sdl:*default-display*) by (* hex-size-multiplier 8))
	  (for rowx from 0)
	  (iter (for y from 0 to (sdl:height sdl:*default-display*) by (* hex-size-multiplier 4))
		(for rowy from 0)
		(when (or (and (evenp rowx) (evenp rowy))
			  (and (oddp rowx) (oddp rowy)))
		  (sdl:draw-surface-at *hex-surface* (sdl:point :x x :y y)))
		)
	  ))
  )

(defmacro with-hexmap ((map width height) &body body)
  `(let ((,map (make-map ,width ,height)))
     ,@body))

(defvar *map* nil)
(defun test ()
  (with-hexmap (*map* 20 20)
  (sdl:with-init ()
    (sdl:window 400 400)
    (setf (sdl:frame-rate) 0)
    (sdl:clear-display sdl:*black*)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display))
      (:idle ()
	     (draw-map *map*)
	     (sdl:update-display))))))


