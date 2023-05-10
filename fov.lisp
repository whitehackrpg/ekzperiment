;;;; ekzperiment.lisp

(in-package #:ekzperiment)

;;;; FoV -----------------------------------------------------------------

(defun make-deg-list (deg inc)
  "Return a list of degrees from a starting point, with an increment."
  (unless (> deg 360) (cons deg (make-deg-list (+ deg inc) inc))))

(defun func-on-radians (deg-lst func fov)
  "Apply a given function (cos or sin) on a list of radians."
  (unless (null deg-lst)
    (cons (* (funcall func (* (car deg-lst) (/ pi 180))) fov)
	  (func-on-radians (cdr deg-lst) func fov))))

(defun make-pair-list (lst fov)
  "Generate a list of cos sin pairs."
  (unless (null lst)
    (let ((lst1 (func-on-radians lst 'cos fov))
	  (lst2 (func-on-radians lst 'sin fov)))
      (remove-duplicates (cons (cons (car lst1) (car lst2))
			       (make-pair-list (cdr lst) fov))))))

(defun generate-fov-hash (inc fov)
  "Return a hash of ray coordinates."
  (let (newlist (hash (make-hash-table :test #'eq)))
    (dolist (pair (make-pair-list (make-deg-list 0 inc) fov))
      (let ((midlist nil))
	(dotimes (z fov)
	  (let ((xvalue (round (* (/ z fov) (car pair))))
		(yvalue (round (* (/ z fov) (cdr pair)))))
	    (push (complex xvalue yvalue) midlist)))
	(push (reverse midlist) newlist)))
    (dolist (key newlist hash)
      (setf (peek key hash) nil))))

(defun field-of-view (lvl &aux list)
  "Ray-casting from pre-calculated hash."
  (loop for lst being the hash-keys in *fovhash* do
    (dolist (c lst)
      (let ((xy (+ (@xy) c)))
	(cond ((or (not (peek >base xy lvl))
		   (not (cwithin xy 0 (car (l-dimension lvl)) 0
				 (cdr (l-dimension lvl))))
		   (and (or (eq (peek >asciibase xy lvl) #\#)
			    (and (not (= xy (@xy)))
				 (eq (peek >asciibase xy lvl) #\+)))
			(push xy list)))
	       (return))
	      (t (push xy list)))))
	finally (return list)))
