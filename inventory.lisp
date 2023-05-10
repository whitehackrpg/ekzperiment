;;;; inventory.lisp

(in-package #:ekzperiment)

;;; Pickup and inventory ---------------------------------------------------------------------

(defun inv-wrapper (lvl)
  "Show the inventory, what is on the ground, and let the player switch items between the two."
  (shade 0.6)
  (draw-lvl  lvl)
  (values (handle-inventory lvl) (setf (blt:layer) 0)
	  (shade 1)))

(let ((xo (+ (- (car *tdims*) 85) 2))
      (yo (floor (- (cdr *tdims*) 25) 2))
      (iscr 0)
      (active-ci 0))

  (defun build-helper (x i c inv color)
    "Helper function to print the inventory."
    (let ((it (nth i inv)))
      (build-canvas
       (list (+ x xo) (+ 5 (+ i yo))
	     (make-string 20 :initial-element #\SPACE)) ; erase old ones
       (list (+ x xo)
	     (+ 5 (+ i yo))
	     (str "  "
		  (if it
		      (string-capitalize (str (i-model it) " "
					      (i-fsymb it)))
		      "...")))
       (list (+ x xo)
	     (+ 5 (+ i yo))
	     (str (if (= i (nth c *li*)) "> " "  "))
	     (when color
	       (cadr *vision*))))))

  (defun draw-inventory (oldlvl)
    (setf xo (if (zerop (hud-p)) 0 (+ (- (car *tdims*) 85) 2))
	  (blt:layer) 1
	  (blt:color) (apply #'blt:rgba (car *vision*)))
    (build-canvas (list (+ 13 xo) (+ yo 3) (mkline 44 #\-))
		  (list (+ 15 xo) (+ yo 3) (case (car *li*)
					     (0 " WEAPON ")
					     (1 " ARMOR ")
					     (t " BACKPACK ")))
		  (list (+ 36 xo) (+ yo 3) (case (cadr *li*)
					     ((0 1) " GROUND ")
					     (t " BURIED "))))
    (dotimes (n 5)
      (build-helper 14 n 0
		    (subseq (e-items @) iscr (+ iscr 5))
		    (zerop active-ci))
      (build-helper 35 n 1 (e-items (peek >loot (@xy) oldlvl (make-e)))
		    (not (zerop active-ci))))
    (setf (blt:layer) 0)
    (release-canvas))

  (defun handle-inventory (oldlvl)
    "Helper input function for pickup: Select different slots in the inventory and on the ground, move things between them and use items. Return a level (usually an altered copy)."
    (draw-inventory oldlvl)
    (let ((inp (blt:read))
	  (a (nth (+ (car *li*) iscr) (e-items @)))
	  (b (when (peek >loot (@xy) oldlvl)
	       (nth (cadr *li*) (e-items (peek >loot (@xy) oldlvl))))))
      (handle-inventory
       (or (case inp ; leave or need to redraw lvl (copy or old)
	     (41 (return-from handle-inventory oldlvl)) ; esc
	     (4 (draw-lvl (switcheroo% a b oldlvl))) ; a, copies
	     (6 (draw-lvl ; c, try to combine items, copies
		 (copylvl% oldlvl (combine-items a b oldlvl))))
	     (7 (mess (gendesc (if (zerop *ci*) a b))) ; d, describe
	      (draw-lvl oldlvl)) ; <- redraw to see description
	     (9 (draw-lvl (use-item% oldlvl nil)))) ; f to use, copies
	   (values oldlvl
		   (case inp
		     (11 (down-to *ci* 0) (down-to active-ci 0))
		     (13 (up-to (nth *ci* *li*)
				4
				(when (zerop *ci*)
				  (up-to iscr (- (length (e-items @))
						 5)))))
		     (14 (down-to (nth *ci* *li*) 0
				  (when (zerop *ci*)
				    (down-to iscr 0))))
		     (15 (up-to *ci* 1) (up-to active-ci 1))))))))

  (defun switcheroo% (a b oldlvl)
    "Destructively switch what is on the ground and what is in the selected inventory slot."
    (when (body-mismatch b)
      (mess (format nil "You can't wear a ~A!" (i-fsymb b)))
      (return-from switcheroo% oldlvl))
    (setf (nth (+ (car *li*) iscr) (e-items @)) b)
    (let ((lvl (copylvl% oldlvl 'new)))
      (if (peek >loot (@xy) oldlvl)
	  (setf (nth (cadr *li*) (e-items (peek >loot (@xy) lvl))) a)
	  (setf (peek >loot (@xy) lvl)
		(make-e :ascii #\! :descr 'loot :remem nil
			:items (cons a (make-list 4)))))
      (when (every #'null (e-items (peek >loot (@xy) lvl)))
	(remhash (@xy) (l-loot lvl)))
      lvl))

  (defun combine-items (item1 item2 oldlvl)
  (let ((newitem (craft2 item1 item2)))
    (when newitem
      (let ((newlvl (copylvl% oldlvl 'new)))
	(setf (nth (cadr *li*) (peek >itemsloot (@xy) newlvl)) nil
	      (nth (+ (car *li*) iscr) (e-items @)) newitem)
	(when (every #'null (peek >itemsloot (@xy) newlvl))
	  (remhash (@xy) (l-loot newlvl)))
	newlvl))))

  (defun body-mismatch (item &optional (templots *li*))
    "Return true if trying to equip armor from an empty groundslot or with a non-armor item."
    (and (= (+ (car templots) iscr) 1)
	 (not (or (null item)
		  (eq (e-fsymb item) 'armor)))))

  (defun item-to-use (&optional hand)
    "Return the item to use (either the selected one if called from the inventory screen, or the item in the protagonists hand.)"
    (or (when hand (car (e-items @)))
	(nth (+ (car *li*) iscr) (e-items @))))

  (defun use-item% (oldlvl &optional hand)
    "Use an item and return an altered copy of the level (or an altered version of the level given as argument, if it is already a copy)."
    (let ((item (item-to-use hand)))
      (if item
	  (cond ((and (null hand)
		      (or-eq (i-fsymb item) 'gun 'key))
		 (values oldlvl
			 (mess
			  (format nil "Can't use a ~A from inventory."
				  (i-fsymb item)))))
		((and item
		      (car (e-items @))
		      (or-eq (i-fsymb item) 'gun 'clip 'potion))
		 (funcall (symbol-function (symb (i-fsymb item) '%))
			  item oldlvl))
		(t oldlvl))
	  oldlvl)))

  (defun remitem (item)
    (decf (i-count item))
  (when (zerop (i-count item))
    (if (eq item (car (e-items @)))
	(setf (car (e-items @)) nil)
	(setf (nth (+ (car *li*) iscr) (e-items @)) nil)))))
