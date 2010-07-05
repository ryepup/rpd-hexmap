;;; -*- mode: lisp; indent-tabs: nil -*-

(in-package :rpd-hexmap)

(defclass hex-map ()
  ((hex-multiplier :accessor hex-multiplier
		   :initform (sdl:point :x 1 :y 1))
   (hexes-per-row :accessor hexes-per-row :initarg
		  :hexes-per-row :initform 5)
   (hexes-per-column :accessor hexes-per-column :initarg
		     :hexes-per-column :initform 5)
   (hexes :accessor hexes :initform nil)
   (hex-surface :accessor hex-surface)
   (tiled-surface :accessor tiled-surface :initform nil)))

(defmethod hex-width ((map hex-map))
  (* (sdl:x (hex-multiplier map))
     (apply #'max (mapcar #'first *unit-hex*))))
(defmethod hex-height ((map hex-map))
  (* (sdl:y (hex-multiplier map))
     (apply #'max (mapcar #'second *unit-hex*))))

(defmacro free-and-clear (thing)
  `(when ,thing
     (sdl:free ,thing)
     (setf ,thing nil)))

(defmethod sdl:free ((map hex-map))
  (free-and-clear (hex-surface map))
  (free-and-clear (tiled-surface map)))

(defvar *unit-hex* '((0 8) (6 16) (16 16) (22 8) (16 0) (6 0) (0 8)))

(defmethod make-tiled-surface ((map hex-map) &key width height)
  (with-accessors ((hsm hex-multiplier)
		   (hex-width hex-width)
		   (hex-height hex-height)
		   (hex-surface hex-surface)
		   (tiled-surface tiled-surface)) map
    (setf hex-surface (sdl:create-surface (1+ hex-width) (1+ hex-height))
	  tiled-surface (sdl:create-surface width height))
    (sdl:draw-polygon (iter (for (x y) in *unit-hex*)
			    (collect (sdl:point :x (* (sdl:x hsm) x)
						:y (* (sdl:y hsm) y))))
		      :surface hex-surface
		      :color sdl:*white* )
    
    (iter (with y-tile-offset = (* (sdl:y hsm) (second (first *unit-hex*))))
	  (with x-tile-offset = (+ hex-width
				   (* (sdl:x hsm)
				      (- (first (third *unit-hex*))
					 (first (second *unit-hex*))))))
	  (with x-odd-row-offset = (* (sdl:x hsm) (first (third *unit-hex*))))
	  (for y from 0 to (sdl:height tiled-surface) by y-tile-offset)
	  (for rowy from 0  to (* 2 (1- (hexes-per-column map))))
	  (iter (for x from (if (evenp rowy) 0 x-odd-row-offset) to
		     (sdl:width tiled-surface) by x-tile-offset)
		(for rowx from 0 to (- (hexes-per-row map) 1
				       (if (evenp rowy) 0 1)))
		(push (list x y) (hexes map))
		(sdl:draw-surface-at-* hex-surface x y
				       :surface tiled-surface)))))

(defmethod draw-map ((map hex-map) &key (x 0) (y 0)
		     (width (sdl:width sdl:*default-display*))
		     (height (sdl:height sdl:*default-display*)))
  (unless (tiled-surface map)
    (setf (sdl:x (hex-multiplier map))
	  (truncate (/ width (* (hexes-per-row map) (hex-width map))))
	  
	  (sdl:y (hex-multiplier map))
	  (truncate (/ height (* (hexes-per-column map) (hex-height map)))))  
    (make-tiled-surface map :width width :height height))
  (sdl:draw-surface-at-* (tiled-surface map) x y))

(defmacro with-hexmap ((map hexes-per-row hexes-per-column) &body body)
  `(let ((,map (make-instance 'hex-map
			      :hexes-per-row ,hexes-per-row
			      :hexes-per-column ,hexes-per-column)))
     (prog1
	 (progn ,@body)
       (sdl:free ,map))))

(defvar *map* nil)
(defun test ()
  (sdl:with-init ()
    (sdl:window 640 480)
    (setf (sdl:frame-rate) 0)      
    (sdl:clear-display sdl:*black*)
    (with-hexmap (*map* 10 14)
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


