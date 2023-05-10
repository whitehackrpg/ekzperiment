;;;; levels.lisp

(in-package #:ekzperiment)


;;; Build levels -------------------------------------------------------------------------
(defun mklevels (number-of &aux (levels (make-hash-table :test #'eq)))
  "Make all the levels."
  (setf (levels 'allstairs) (make-hash-table :test #'equal)
	(levels 'allcounts) (make-hash-table :test #'eq))
  (dotimes (level number-of (mkallstairs number-of levels))
    (destructuring-bind (x1 x2 y1 y2) *mdims*
      (let* ((x (+ x1 (random (- x2 x1))))
	     (y (+ y1 (random (- y2 y1))))
	     (things (floor (* x y) (+ 80 (random 50)))))
	(setf (levels level)
	      (make-l :dimension (cons x y)
		      :number level :base (mkdoors level (mkmap x y))
		      :seen (make-hash-table)
		      :exit (make-hash-table)
		      :mobs (make-hash-table)
		      :loot (make-hash-table)))
	(mkmobs things (mkloot things level (levels level)))))))

(defun mkdoors (lnum hash &aux (newhash (make-hash-table)))
  "Make the doors in a map."
  (flet ((tile (n) (peek >ascii n hash (make-e))))
    (loop for k being the hash-keys in hash using (hash-value v) do
      (if (and (= 4 (count #\. (mapcar #'(lambda (n) (tile (+ k n)))
				       *surr*)))
	       (or (mq #\# (tile (+ k #C(1 0))) (tile (+ k #C(-1 0))))
		   (mq #\# (tile (+ k #C(0 1))) (tile (+ k #C(0 -1)))))
	       (zerop (random 4)))
	  (setf (peek k newhash) (make-e :ascii #\+
					 :units (+ (random 6) lnum)))
	  (setf (peek k newhash) v))
	  finally (return newhash))))

(defun surr (loc &optional lev coords)
  "Return the characters around a position, either in the BLT grid, or, if lev is given, in a level's base-slot."
  (loop for n in *surr*
	collect
	(cond ((and lev coords)
	       (cons (peek >asciibase (+ loc n) lev (make-e :ascii #\p))
		     (+ loc n)))
	      (lev (peek >asciibase (+ loc n) lev (make-e :ascii #\p)))
	      (t (cello (+ loc n))))))

(defun getloc (lev)
  "Return a non-wall position that has at least one adjacent non-wall position."
  (let ((rloc (rdim (car (l-dimension lev)) (cdr (l-dimension lev)))))
    (if (and (member #\. (surr rloc lev))
	     (not (or-eq (peek >asciibase rloc lev) #\# #\+)))
	rloc
	(getloc lev))))

(defun mkallstairs (number-of levels)
  "Make all stairs between levels."
  (labels ((setboth (a b)
	     (let ((r1 (getloc (levels a))) (r2 (getloc (levels b))))
	       (setf (peek (cons a r1) (levels 'allstairs)) (cons b r2)
		     (peek (cons b r2) (levels 'allstairs))
		     (cons a r1)))))
    (dotimes (n number-of)
      (if (= n (1- number-of)) (setboth n 0) (setboth n (1+ n))))
    (loop for k being the hash-keys in (levels 'allstairs)
	    using (hash-value v) do
	      (setf (peek (cdr k) (l-exit (levels (car k))))
		    (make-e :remem nil
			    :ascii (if (> (car k) (car v)) #\< #\>)))
	  finally (return-from mkallstairs levels))))

(defun mkmobs (n lvl)
  "Make and place the mobs on a level."
  (dotimes (not-used n lvl)
    (setf (peek >mobs (getloc lvl) lvl)
	  (make-m :ascii #\z :units 10 :remem nil :fsymb 'mobattack
		  :smart (random 10)))))

(defun mkloot (n lnum lvl)
  "Make and place the loot on a level."
  (dotimes (not-used (1+ (random n)) lvl)
    (setf (peek >loot (getloc lvl) lvl)
	  (make-e :ascii #\! :descr 'loot :remem nil
		  :items (cons (genitem (rel (expand '((clip 10) (key 6)
						    gun (potion 3))))
					lnum)
			     (make-list 4 :initial-element nil))))))

(defun genitem (type lnum)
  "Generate an item of type, with some random and some none-random qualities."
  (let ((d (rel (list "This is a ~A ~A with ~A ~A, maximum ~A ~A."
		      "This ~A ~A has ~A ~A. The highest ~A is ~A."
		      "A handy ~A ~A, remaining ~A ~A. Top ~A is ~A."))))
    (case type
      (bat (make-i :model 'spiked
		   :fsymb type
		   :units 20
		   :uterm 'damage
		   :count 1
		   :descr "Your trusty ~A ~A. ~A There's only ~A of these. Top ~A is ~A."))
      (armor (make-i :model (rel '(makeshift))
		     :fsymb type
		     :count (1+ (random 2))
		     :cterm 'grade
		     :units (1+ (random 6))
		     :uterm 'protection
		     :descr "This is ~A ~A, ~A ~A. Max ~A is ~A."))
      (potion (make-i :model (rel '(healing corrosive curing))
		      :fsymb type
		      :count (1+ (random 3))
		      :cterm 'doses
		      :units (1+ (random 8))
		      :uterm 'efficiency
		      :descr d))
      (gun (make-i :model (rel '(browning luger sig-sauer))
		   :fsymb type
		   :count (1+ (random 15))
		   :cterm 'rounds
		   :units (1+ (random 10))
		   :uterm 'damage
		   :descr d))
      (clip (make-i :model 'bullet
		    :fsymb type
		    :units nil
		    :count (1+ (random 10))
		    :descr "This is a ~A ~A with ~A~A remaining rounds."))
      (key (make-i :model (rel '(card metal))
		   :fsymb 'key
		   :count (+ (random 5) lnum)
		   :cterm 'access-level
		   :units (1+ (random 10))
		   :uterm 'durability
		   :descr "This is a ~A ~A with ~A ~A.")))))

(defun gendesc (s)
  "Generate a description string from the slot values of an e-struct s."
  (if (null s)
      "..."
      (format nil (i-descr s) (if (eq (i-fsymb s) 'gun)
				(string-capitalize (i-model s))
				(strdc (i-model s)))
	      (strdc (i-fsymb s)) (strdc (i-cterm s))
	      (i-count s) (strdc (i-uterm s)) (i-units s))))

(defun mkmap (xdim ydim &aux (map (make-hash-table)))
  "Make a map (the base-layer of a level)."
  (dotimes (x xdim)
    (dotimes (y ydim)
      (setf (peek (complex x y) map) (make-e :ascii #\# :count 0))))
  (genrooms xdim ydim map (complex (round xdim 2) (round ydim 2)) nil
	    (floor (* xdim ydim) 50)))

(defun leg (a end-a b map &optional v)
  "Make a leg in a corridor -- give a fifth arg for a vertical leg."
  (unless (= a end-a)
    (setf (peek (complex (if v b a) (if v a b)) map)
	  (make-e :ascii #\. :count 0))
    (leg (toward a end-a) end-a b map v)))

(defun make-corridor (nc lc map)
  "Make a corridor between two rooms."
  (destructuring-bind (nx lx ny ly) (list (realpart nc) (realpart lc)
					  (imagpart nc) (imagpart lc))
    (case (random 2)
      (0 (leg nx lx ny map) (leg ny ly lx map 'vert))
      (t (leg ny ly lx map 'vert) (leg nx lx ny map)))))

(defun o-o-bounds (xdim ydim cx cy width height)
  "Check if room is out of bounds."
  (dotimes (w width)
    (dotimes (h height)
      (unless (and (within (+ w cx) 1 (- xdim 2))
		   (within (+ h cy) 1 (- ydim 2)))
	(return-from o-o-bounds t)))))

(defun genrooms (xdim ydim map old-cent room-list count)
  "Generate the rooms of a level."
  (let* ((w (+ 3 (random 6))) ; width
	 (h (+ 3 (random 6))) ; height
	 (cx (random xdim))   ; corner-x
	 (cy (random ydim))   ; corner-y
	 (new-cent (complex (+ cx (round w 2)) (+ cy (round h 2)))))
    (cond ((zerop count) map)
	  ((and (not (o-o-bounds xdim ydim cx cy w h))
		(null (intersection (rcoords w h cx cy) room-list)))
	   (dotimes (x w)
	     (dotimes (y h)
	       (let ((tile-xy (complex (+ x cx) (+ y cy))))
		 (setf (peek tile-xy map) (make-e :ascii #\. :count 0))
		 (push tile-xy room-list))))
	   (make-corridor new-cent old-cent map)
	   (genrooms xdim ydim map new-cent room-list (1- count)))
	  (t (genrooms xdim ydim map old-cent room-list (1- count))))))

(defun rcoords (width height corner-x corner-y &aux temp-room)
  "Generate a list of coordinates for a room."
  (dotimes (x width temp-room)
    (dotimes (y height)
      (let ((col (+ x corner-x))
	    (row (+ y corner-y)))
	(push (complex col row) temp-room)))))
