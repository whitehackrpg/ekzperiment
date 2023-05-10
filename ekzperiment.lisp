;;;; ekzperiment.lisp

(in-package #:ekzperiment)


;;; Character stuff --------------------------------------------------------------------
(defparameter @
    (make-m :ascii #\@ :count 1 :units 4000 :fsymb '@attack :taint 0
	    :extra nil; (list 'a 'b 'c)
	    :items (append (list (genitem 'bat 1) (genitem 'armor 1)
				 (genitem 'potion 1)
				 (genitem 'clip 1) (genitem 'key 1)
				 (genitem 'potion 1))
			   (make-list 5)))
  "The protagonist global variable (without earmuffs).")

(let ((xy 0))
  (defun @xy (&optional n reset)
    "Return @ position, or if given n, modify the @ position, or if given n and reset, set position to n."
    (cond ((and reset n) (setf xy n)) 
	  (n (incf xy n))
	  (t xy))))


;;; The Game -----------------------------------------------------------------------
(let ((stage 0))
  (defun stage (&optional num)
    "Closure to keep track of game stages."
    (if num (setf stage num) stage)))

(defun new-lvl (lvl levels)
  "Store the old level and draw the new one."
  (let ((dest (peek (cons (l-number lvl) (@xy)) (levels 'allstairs))))
    (setf (peek (l-number lvl) (levels 'allcounts)) (e-count @)
	  (e-count @) (or (peek (car dest) (levels 'allcounts)) 1))
    (set-offset (@xy (cdr dest) t))
    (setfov% (levels (car dest)))
    (@xy (cdr dest) t)
    (draw-lvl (levels (car dest)))))

(defun configure ()
  "Configure BearLibTerminal and set the field of view range."
  (blt:set "font: ~A, size=~A" *font* *font-size*) 
  (blt:set "window.size = ~AX~A" (car *tdims*) (cdr *tdims*))
  (blt:set "window.title = SBCL + BLT")
  (blt:set "window.fullscreen = true")
  (blt:set "input.mouse-cursor = false")
  (blt:set "output.vsync = true")
  (setf *fovhash* (generate-fov-hash 1 5))
  (mess (format nil "Use h/j/k/l/y/u/n/m to move, t to target, f to ~
                     use, a to pickup and c to toggle the hud.")))

(defun main (&optional (levels (mklevels 10)))
  "Main. Loops local function tick until the game ends."
  (labels ((tick (lvl)
	     (case (stage)
	       (0 (when (> (m-taint @) 0) (decf (e-units @) (m-taint @)))
		(when (m-extra @)
		  (if (zerop (car (m-extra @)))
		      (setf (m-extra @) nil)
		      (decf (car (m-extra @)))))
		(tick (draw-lvl (setfov% (mobs% (copylvl% lvl (input lvl)))))))
	       (1 (stage 0) (tick (new-lvl lvl levels)))
	       (2 (stage 0) (if (eq (save-game levels lvl) 'no)
				(tick lvl)
				'saved))
	       (3 (stage 0) (when (probe-file "savefile")
			      (multiple-value-bind (a b) 
				  (restore-game "savefile")
				(setf levels a lvl b))
			      (delete-file "savefile"))
		(tick lvl)))))
    (catch 'end-game
      (blt:with-terminal
	(configure)
	(cond ((and (probe-file "savefile")
		    (blt-y-or-n-p "Found savefile. Use it?"))
	       (stage 3)
	       (tick nil))
	      (t (set-offset (@xy (getloc (levels 1)) t))
		 (tick (draw-lvl (setfov% (levels 1))))))))))

(defun save-game (levels lvl)
  "Save the game using cl-store. levels lvl @ *fovhash* *messages* and the @ position get saved."
  (cond ((blt-y-or-n-p "Save and quit?")
	 (cl-store:store (list levels lvl @ *fovhash*
			       *messages* (@xy))
			 "savefile")
	 (throw 'end-game 'saved))
	(t (values 'no (draw-lvl  lvl)))))

(defun restore-game (file)
  "Read a savefile and restore the data."
  (let ((sv (cl-store:restore file)))
    (set-offset (@xy (nth 5 sv) t))
    (setf @ (nth 2 sv)
	    *fovhash* (nth 3 sv)
	    *messages* (nth 4 sv))
    (values (nth 0 sv) (draw-lvl (nth 1 sv)))))

(defun setfov% (lvl)
  "Transform the level by changing the e-count of tiles within the field of view."
  (incf (e-count @))
  (dolist (coord (field-of-view lvl) lvl)
    (setf (peek >countbase coord lvl) (e-count @)
	  (peek >seen coord lvl) (peek >base coord lvl))))


(defun handle-keys (list access-level &aux ns)
  "Return a list of positions where there are keys in the protagonist inventory."
  (dotimes (n (length list) (reverse ns))
    (if (and (nth n list)
	     (eq (i-fsymb (nth n list)) 'key)
	     (>= (i-count (nth n list)) access-level))
	(push n ns))))

(defun door (xy lvl)
  "Handle doors. If one unlocks, return a cons of its location and e-struct."
      (let ((keys (handle-keys (e-items @) (peek >unitsbase xy lvl))))
	(cond ((null keys)
	       (mess (format nil "This door has access level ~A. You don't have a key for it."
			     (peek >unitsbase xy lvl)))
	       (@xy))
	    (t 
	      (let ((freshdoor (copy-e (peek >base xy lvl))))
		(setf (e-extra freshdoor) 'unlocked
		      (e-ascii freshdoor) #\/)
		(cons xy freshdoor))))))


(defun retcoord ()
  (blt:key-case
   (blt:read) (:h (complex -1 0)) (:l (complex 1 0)) (:j (complex 0 1))
   (:k (complex 0 -1)) (:y (complex -1 -1)) (:u (complex 1 -1))
   (:b (complex -1 1)) (:n (complex 1 1))))

(defun close-door (lvl)
  (let ((ods (remove-if-not #'(lambda (n) (eq n #\/)) (surr (@xy) lvl 'with-coords)
			    :key #'car)))
    (flet ((cldoor (coord)
	     #'(lambda (l)
		 (setf (peek >asciibase coord l) #\+
		       (peek >extrabase coord l) nil))))
      (cond ((null ods) lvl)
	    ((= (length ods) 1) (cldoor (cdar ods)))
	    (t (box-mess "Please give the direction for the door you want to close.")
	       (blt:refresh)
	       (let ((inp (retcoord)))
	       (cldoor (+ inp (@xy)))))))))

	     
(defun kick (lvl)
  (box-mess "Kick in what direction?")
  (blt:refresh)
  (let* ((inp (retcoord))
	 (newcoord (+ (or inp 0) (@xy))))
    (unless (null inp)
      #'(lambda (l) (when (eq (peek >asciibase newcoord lvl) #\+)
		      (boom (random 15))
		      (if (<= (peek >unitsbase newcoord lvl) 0)
			  (setf (peek >asciibase newcoord l) #\%
				(peek >extrabase newcoord l) 'broken)
			  (decf (peek >unitsbase newcoord l) (random 10))))))))
		    
(defun input (lvl)
  "Get input and either act on it, set a certain stage or return a working modification to the protagonist's position."
  (when (<= (e-units @) 0) (throw 'end-game 'death))
  (blt:key-case
   (blt:read)
   (:escape (when (blt-y-or-n-p "Really quit?")
	      (throw 'end-game 'bye)))
   (:e (values 0 (move-hud lvl)))
   (:p (values 0 (break))) ; Pause for debugger and REPL
   (:d (values 0 (adjust-color-screen lvl)))
   (:a (values (inv-wrapper lvl) (skip-mobs)))
   (:g (values 0 (let ((tile (peek >exit (@xy) lvl)))
		   (if (and tile (or-eq (e-ascii tile) #\> #\<))
		       (stage 1)))))
   (:s (values 0 (stage 2)))
   (:w (values 0 (when (and (probe-file "savefile")
			    (blt-y-or-n-p "Found savefile. Use it? (This game will be lost.)"))
		   (stage 3))))
   (:t (values 0 (target-mob lvl) (skip-mobs)))
   (:f (use-item% lvl 'hand))
   (:c (close-door lvl))
   (:r (values 0 (cond ((zerop (hud-p))
			(set-offset (@xy))
			(hud-p 1))
		       (t (set-offset (@xy) 2 2)
			  (hud-p 0)))))
   ;; This can return a cons (loc . e), a function, or a coord.
   (:k (move #C( 0 -1) lvl)) (:j (move #C(0  1) lvl))
   (:x (kick lvl))
   (:h (move #C(-1  0) lvl)) (:l (move #C(1  0) lvl))
   (:y (move #C(-1 -1) lvl)) (:u (move #C(1 -1) lvl))
   (:b (move #C(-1  1) lvl)) (:n (move #C(1  1) lvl))))

(defun move (n oldlvl)
  (let ((newxy (+ (@xy) n)))
    (cond ((or (zerop n)
	       (null (cello newxy))
	       (eq (cello newxy) #\#))
	   (@xy)) ; ret loc
	  ((peek >mobs newxy oldlvl)
	   (@attack% newxy)) ; ret destructive lambda
	  ((and (eq (peek >asciibase newxy oldlvl) #\+)
		(not (eq (peek >extrabase newxy oldlvl) 'unlocked)))
	   (door newxy oldlvl)) ; ret loc (no key) or (cons xy tile w. door)
	  (t newxy))))

(defun noticep (xy mob lvl)
  "Return an a-star path either if there is a gunshot that the mob hears (plot a new route), or if there isn't a gundshot but the mob is already en route toward the last source of the sound."
  (or (when *loud-sound*
	(let ((distance (abs (- xy (@xy)))))
	  (if (> (random (+ *loud-sound* 2)) ; loud-sound can be zero
		 (floor (* (realpart distance)
			   (imagpart distance))
			(or (m-aware mob) 1)))
	      (setf (m-extra mob) (a-star xy (@xy) lvl)))))
      (m-extra mob)))

(defun rmove (xy lvl)
  (let ((randloc (+ xy (complex (1- (random 3)) (1- (random 3))))))
    (when (and (or (eq (peek >asciibase randloc lvl) #\.)
		   (eq (peek >extrabase randloc lvl) 'unlocked))
	       (null (peek >mobs randloc lvl)))
      randloc)))

(let (skip)
  (defun mobs% (lvl)
    "Unless skip is set (in the closure), try to move mobs who are in FoV, and if they can reach @, attack."
    (unless skip
      (maphash
       #'(lambda (xy mob)
	   (cond ((or (= (e-count @) (peek >countbase xy lvl))
		      (> (m-smart mob) (sqrt (manhattan xy (@xy)))))
		  (if (some #'(lambda (n) (= (- (@xy) xy) n)) *surr*)
		      (mobattack% mob (l-number lvl))
		      (let* ((carast (car (a-star xy (@xy) lvl)))
			     (dest (if carast
				       (node-xy carast)
				       (rmove xy lvl))))
			(when dest
			  (remhash xy (l-mobs lvl))
			  (setf (peek >mobs dest lvl) mob)))))
		 ((noticep xy mob lvl)
		  (let ((dest (node-xy (pop (m-extra mob)))))
		    (unless (peek >mobs dest lvl) ; don't step on fellows
		      (remhash xy (l-mobs lvl))
		      (setf (peek >mobs dest lvl) mob))))
		 (t)))
       (l-mobs lvl))
      (setf *loud-sound* nil))
    (setf skip nil)
    lvl)

  (defun skip-mobs (&optional manual)
    "Switch to skip mobs (useful after some character actions that don't take a game turn."
    (if manual
	(setf skip t)
	(setf skip (if skip nil t)))))

(let ((count 0) target)
  (defun target-mob (lvl)
    "Target a new mob in FoV. A closure remembers the last target."
    (let (mobs)
      (loop for k being the hash-keys in (l-mobs lvl) do
	(when (= (peek >countbase k lvl) (e-count @))
	  (push k mobs)))
      (when mobs (setf target (or (nth count mobs) (car mobs))
		       (blt:color) (blt:red)
		       (cello target) (peek >asciimobs target lvl))
	    (blt:refresh)
	    (case (blt:read)
	      (9 (skip-mobs) (use-item% lvl 'hand)) ; pressed 'f'
	      (23 (setf (blt:color) (blt:white) ; pressed 't'
			(cello target) (peek >asciimobs target lvl))
	       (if (nth (1+ count) mobs)
		   (incf count)
		   (setf count 0))
	       (target-mob lvl))))))
  
  (defun target (&optional n)
    "Helper function for target-mob."
    (if n (setf target nil) target)))


(defun @attack% (target)
  "Return an @ melee attack lambda."
  #'(lambda (lvl)
      (let ((weapon (car (m-items @))))
	(decf (peek >unitsmobs target lvl)
	      (if (and weapon (or-eq (i-fsymb weapon) 'bat))
		  (1+ (random (i-units weapon)))
		  2)))
      (when (<= (peek >unitsmobs target lvl) 0)
	(splatter% lvl target (peek >mobs target lvl))
	(remhash target (l-mobs lvl)))
      lvl))

(defun mobattack% (mob lvl)
  "Return a mob attack lambda."
  (when (zerop (random 5))
    (mess (rel (list "Hzzzzzz!" "Ngng ..."
		     "Brainzzz!" "Mommmmmy!"))))
  (when (zerop (random 20))
    (setf (m-taint @) (if (> (m-taint mob) (m-taint @))
			  (m-taint mob)
			  (1+ (m-taint @))))
    (mess "You got tainted."))
  (decf (e-units @) (+ (random 3) lvl))
  (when (<= (e-units @) 0) (throw 'end-game 'death)))
