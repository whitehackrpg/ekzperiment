;;;; item-handling.lisp

(in-package #:ekzperiment)

;;; Item-handling

(defun potion% (item oldlvl)
  "Handle potions (throwing, healing and non-healing)."
  (remitem item)
  (skip-mobs)
  (funcall (if (target) #'throw-potion #'drink) item oldlvl))

(defun throw-potion (item oldlvl)
  (mess (format nil "You throw the ~A." (i-fsymb item)))
  (shot2 (@xy) (target) oldlvl 'throw) ; avoid boom message
  (values
   (copylvl% oldlvl
	     (case (i-model item)
	       (corrosive #'(lambda (l)
			      (mess "The zombie is destroyed.")
			      (remhash (target) (l-mobs l))))
	       (healing #'(lambda (l)
			    (mess "You aggravate the zombie")
			    (incf (m-smart (peek >mobs (target) l)) 20)))
	       (t #'(lambda (l)
		      (mess "You calm the zombie down.")
		      (setf (m-smart (peek >mobs (target) l)) 0)))))
   (target t)))

(defun drink (item oldlvl)
  (flet ((hp? (f string)
	   (let ((e (1+ (random (i-units item)))))
	     (incf (e-units @) (funcall f e))
	     (mstr (> e 1) (format nil string e)))))
    (mess (case (i-model item)
	    (healing (hp? #'+ "You gained ~A HP"))
	    (corrosive (hp? #'- "Don't *drink* that! You lost ~A HP"))
	    (t (decf (m-taint @) (i-units item))
	     "The virus levels recede.")))
    oldlvl))
	       
(defun on-ground (item loc lvl)
  (unless (null item)
    (if (null (peek >loot loc lvl))
	#'(lambda (l) (setf (peek >loot loc l)
			    (make-e :ascii #\! :descr 'loot :remem nil
				    :items (cons (copy-i item)
						 (make-list 4)))))
	(let ((slot (position-if #'null (peek >itemsloot loc lvl))))
	  (if (null slot)
	      (mess "Something is lost in the rubble.")
	      (let* ((newlist (copy-list (peek >itemsloot loc lvl))))
		(setf (nth slot newlist) (copy-i item))
		#'(lambda (l) (setf (peek >itemsloot loc l) newlist))))))))

;; Ruins potions doesn't work, and also -- sometimes something should perhaps be left on the ground, make a sound etc -- i.e. what is made has an effect on the game world. combine items should return what is placed, not remhash anything. (but it returns a new level)

(defun craft2 (item1 item2)
  (case (find-combo item1 item2)
    (clip (incf (i-count item1) (i-count item2))
     item1)
    (potion (cond ((eq (i-model item1) (i-model item2))
		   (incf (i-count item1) (i-count item2))
		   item1)
		  ((every #'(lambda (n) (member n (list 'healing
							'corrosive)))
			  (list (i-model item1) (i-model item2)))
		   (make-i :model 'chemical :fsymb 'bomb
			   :descr "Careful! That's a chemical bomb!"))
		  (t (mess "You can't combine those.")
		     nil)))
;    (bomb)
 ;   (armor)
    (t (mess "You can't combine those.")
     nil)))

(defun craft (item1 item2)
  (cond ((eq (i-fsymb item1) (i-fsymb item2))
	 (let ((newitem (copy-i item1)))
	   (incf (i-count newitem) (i-count item2))
	   newitem))
	(t (mess "You can't combine those."))))

(defun clip% (item oldlvl)
  "Use a clip to recharge a weapon."
  (when (target)
    (mess "You throw the clip, but it does no good. It bounces off your enemy and lands on the ground.")
    (let ((newlvl (copylvl% oldlvl (on-ground item (target) oldlvl))))
      (shot2 (@xy) (target) oldlvl 'throw)
      (setf (car (e-items @)) nil)
      (target t)
      (return-from clip% newlvl)))
  (let ((inhand (car (e-items @))))
    (when (and inhand (eq (i-fsymb inhand) 'gun))
      (mess "You load your gun.")
      (incf (i-count (car (e-items @)))
	    (i-count item))
      (setf (nth (car *li*) (e-items @)) nil))) ; add crap on ground?
  oldlvl)

(defun gun% (item oldlvl)
  "Fire a gun, if there is ammo."
  (cond ((not (target)) (skip-mobs))
	((zerop (i-count item))
	 (mess "Click!"))
	(t (shot2 (@xy) (target) oldlvl)
	   (decf (i-count (car (e-items @))))
	   (let* ((lvl (copylvl% oldlvl 'new))
		  (newhp (- (peek >unitsmobs (target) lvl)
			    ;; Raised gun damage to instakill (100)
			    (+ 1 100 (random (i-units item))))))
	     (if (<= newhp 0)
		 (remhash (target) (l-mobs lvl))
		 (setf (peek >unitsmobs (target) lvl) newhp))
	     (target t)
	     lvl))))

(defun mess (string &optional col)
  "Append a new message to the messages."
  (let* ((new (reverse (mapcar #'(lambda (n) (rempar (apply #'str n)))
				   (linebreaker 27 string))))
	 (newcol (when col (mapcar #'(lambda (n) (cons col n)) new))))
    (setf *messages* (append (or newcol new) *messages*)))
  nil)

(defun splatter% (lvl target mob &optional (splatter-rating 1))
  (mapcar #'(lambda (xy)
	      (let ((tile (peek >asciibase (+ xy target) lvl)))
		(when (and tile (< (random 5) splatter-rating))
		  (when (eq (peek >asciibase (+ xy target) lvl) #\.)
		    (setf (peek >asciibase (+ xy target) lvl)
			  (rel (expand (list #\' #\* (list #\, 4) #\'
					     #\} #\{ #\~ #\&)))))
		  (when (= (+ xy target) (@xy))
		    (setf (m-taint @) (if (> (m-taint mob) (m-taint @))
					  (m-taint mob)
					  (1+ (m-taint @)))
			  (m-extra @) '(5 200 30 30))
		    (mess "You got tainted."))
		  (setf (peek >extrabase (+ xy target) lvl)
			(list 200 30 30)))))
	  (cons 0 *surr*)))

(defun boom (value)
  (setf *loud-sound* value)
  (mess (format nil "BOOM! Someone ~A have heard that."
		(if (> value 10) "is likely to" "may"))))
								  
(defun shot2 (start end lvl &optional throw)
  (unless throw
    (boom 50))
  (let ((difference (- end start))
	(middle (find-middle start end)))
    (cond ((or (> (abs (realpart difference)) 2)
	       (> (abs (imagpart difference)) 2))
	   (shot (a-star start middle lvl))
	   (shot (a-star middle end lvl)))
	  (t (shot (a-star start end lvl))))))

(defun shot (path)
  "Trace a shot from @ to the target."
  (let ((n 1))
    (dolist (seg (mapcar #'node-xy path) (blt:refresh))
      (setf (blt:color) (apply #'blt:rgba
			       (list 255 (round (* 155 (/ 1 n))) 0))
	    (cello seg) #\*)
      (incf n)))
  (setf (blt:color) (apply #'blt:rgba (car *vision*)))
  (sleep 0.03))
  
