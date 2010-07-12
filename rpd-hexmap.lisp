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

(defvar *unit-hex* '((0 8) (6 16) (16 16) (22 8) (16 0) (6 0) (0 8)))

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

;; #2A ((1 0 1 0 1 0)
;;      (0 1 0 1 0 1)
;;      (1 0 1 0 1 0))
(defun %make-hex-grid (hexes-per-row hexes-per-column)
  (let ((grid (make-array (list (* 2 hexes-per-column)
				(* 2 hexes-per-row)))))
    (dotimes (x (array-dimension grid 1))
      (dotimes (y (array-dimension grid 0))
	(when (or (and (evenp y) (evenp x))
		  (and (oddp y) (oddp x)))
	  (setf (aref grid y x) 1))))
    grid))

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

    (iter
      (with grid = (%make-hex-grid (hexes-per-row map) (hexes-per-column map)))
      (with y-tile-offset = (* (sdl:y hsm) (second (first *unit-hex*))))
      (with x-tile-offset = (* (sdl:x hsm) (first (third *unit-hex*))))
      (for rowy from 0 to (1- (array-dimension grid 0)))
      (for y from 0 by y-tile-offset)
      (iter (for rowx from 0 to (1- (array-dimension grid 1)))
	    (for x from 0 by x-tile-offset)
	    (when (plusp (aref grid rowy rowx))
	      ;;save these coords in a grid on the map so we can do adjacency later
	      (sdl:draw-surface-at-* hex-surface x y
				     :surface tiled-surface))))))

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
    (setf (sdl:frame-rate) 20)      
    (sdl:clear-display sdl:*black*)
    (with-hexmap (*map* 5 7)
      (draw-map *map*)
      (sdl:with-events ()
	(:quit-event () t)   
	(:video-expose-event () (sdl:update-display))
	(:idle ()
	       (sdl:update-display))))))


