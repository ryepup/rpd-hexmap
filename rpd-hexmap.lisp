;;; -*- mode: lisp; indent-tabs: nil -*-

(in-package :rpd-hexmap)

(defclass hex-map ()
  ((hex-multiplier :accessor hex-multiplier
		   :initarg :hex-multiplier
		   :initform 1)
   (hexes :accessor hexes :initform nil)
   (hex-surface :accessor hex-surface)
   (tiled-surface :accessor tiled-surface :initform nil)))
(defmethod hex-width ((map hex-map))
  (* (hex-multiplier map)
     (apply #'max (mapcar #'first *unit-hex*))))
(defmethod hex-height ((map hex-map))
  (* (hex-multiplier map)
     (apply #'max (mapcar #'second *unit-hex*))))

(defmacro free-and-clear (thing)
  `(when ,thing
     (sdl:free ,thing)
     (setf ,thing nil)))

(defmethod sdl:free ((map hex-map))
  (free-and-clear (hex-surface map))
  (free-and-clear (tiled-surface map)))

(defvar *unit-hex* '((0 8) (6 16) (16 16) (22 8) (16 0) (6 0) (0 8)))

(defmethod make-tiled-surface ((map hex-map))
  (with-accessors ((hsm hex-multiplier)
		   (hex-width hex-width)
		   (hex-height hex-height)
		   (hex-surface hex-surface)
		   (tiled-surface tiled-surface)) map
    (setf hex-surface (sdl:create-surface (1+ hex-width) (1+ hex-height))
	  tiled-surface (sdl:create-surface (sdl:width sdl:*default-display*)
					    (sdl:height sdl:*default-display*)))
    (sdl:draw-polygon (iter (for (x y) in *unit-hex*)
			    (collect (sdl:point :x (* hsm x)
						:y (* hsm y))))
		      :surface hex-surface
		      :color sdl:*white* )
    
    (iter (with y-tile-offset = (* hsm (second (first *unit-hex*))))
	  (with x-tile-offset = (+ hex-width
				   (* hsm
				      (- (first (third *unit-hex*))
					 (first (second *unit-hex*))))))
	  (with x-odd-row-offset = (* hsm (first (third *unit-hex*))))
	  (for y from 0 to (sdl:height tiled-surface) by y-tile-offset)
	  (for rowy from 0)
	  (iter (for x from (if (evenp rowy) 0 x-odd-row-offset) to
		     (sdl:width tiled-surface) by x-tile-offset)
		(push (list x y) (hexes map ))
		(sdl:draw-surface-at-* hex-surface x y
				       :surface tiled-surface)))))

(defmethod draw-map ((map hex-map))
  (unless (tiled-surface map) (make-tiled-surface map))
  (sdl:draw-surface-at-* (tiled-surface map) 0 0))

(defmacro with-hexmap ((map hex-multiplier) &body body)
  `(let ((,map (make-instance 'hex-map
			      :hex-multiplier ,hex-multiplier)))
     (prog1
	 (progn ,@body)
       (sdl:free ,map))))

(defvar *map* nil)
(defun test ()
  (sdl:with-init ()
    (sdl:window 400 400)
    (setf (sdl:frame-rate) 0)      
    (sdl:clear-display sdl:*black*)
    (with-hexmap (*map* 2)
      (draw-map *map*)
      (sdl:with-events ()
	(:quit-event () t)
	(:MOUSE-MOTION-EVENT (:STATE STATE :X X :Y Y :X-REL X-REL :Y-REL Y-REL)
			     (declare (ignore state x-rel y-rel))
			     (draw-map *map*)
			     (sdl:draw-pixel (sdl:point :x x :y y)
					     :color sdl:*red*)) 
	(:video-expose-event () (sdl:update-display))
	(:idle ()
	       (sdl:update-display))))))


