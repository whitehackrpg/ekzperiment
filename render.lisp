;;;; render.lisp

(in-package #:ekzperiment)

;;; Render functions ---------------------------------------------------------------------
(let ((x 0) (y 0) (a 0) (b 0))
  (defun camera (mod)
    "Scroll the display by following @ with a camera, whose lens is defined by *cam*."
    (if (< (abs (+ a (realpart mod))) 5)
      (incf a (realpart mod))
      (incf x (realpart mod)))
    (if (< (abs (+ b (imagpart mod))) 5)
      (incf b (imagpart mod))
      (incf y (imagpart mod))))
  
  (defun offset (n)
    "Return a new offset."
    (+ n (complex x y)))

  (defun set-offset (n &optional (m 3) (l 2))
    "Set the offset."
    (setf a 0
	  b 0
	  x (- (1- (floor (car *tdims*) m)) (realpart n))
	  y (- (1- (floor (cdr *tdims*) l)) (imagpart n)))))

(let ((hud-p 1) (pointer 0))
  
  (defun messages ()
    "Print the messages."
    (dotimes (n 10)
      (let ((message (nth n *messages*)))
	(when (consp message)
	  (setf (blt:color) (apply #'blt:rgba (car message))
		message (cdr message)))
	(blt:print (caddr *hxy*) (+ (cadddr *hxy*) (abs (- n 10)))
		   message)
	(setf (blt:color) (apply #'blt:rgba
				 (modrgb .7 (car *vision*)))))))
  
  (defun str+cnt (fn1 fn2 item)
    "Return a string where a countable item gets a (count) appended to its name."
    (if (null item)
	"Nothing"
	(string-capitalize
	 (str (funcall fn1 item)
	      (let ((cnt (funcall fn2 item)))
		(if (numberp cnt)
		    (format nil " (~A) " cnt)
		    ""))))))
  
  (defun hud (lvl)
    "Display the hud."
    (cond ((zerop hud-p)
	   (blt:refresh))
	  (t (setf (blt:color)
		   (apply #'blt:rgba (modrgb .7 (car *vision*))))
	     (build-canvas
	      (list (car *hxy*) (cadr *hxy*) "---- Ekzperimental HUD ---"
		    (car *vision*))
	      (list (nth 4 *hxy*) (nth 5 *hxy*)
		    (str "LVL " (l-number lvl) "  HP " (e-units @)
			 "  XY "
			 (rempar (str (realpart (@xy)) ":"
				      (imagpart (@xy))))))
	      (list (nth 6 *hxy*) (+ (nth 7 *hxy*) 2)
		    (rempar (str "ADJACENT " (surr (@xy))))))
	     (dotimes (n 2) ; Print what is in weapon and armor slots
	       (add-to-canvas (list (car *hxy*) (+ (cadr *hxy*) (+ 4 n))
				   (str+cnt #'i-model #'i-count
					    (nth n (e-items @))))))
	     (messages)
	     (release-canvas))))

  (defun move-hud (lvl &optional noq)
    "Move the elements of the HUD (find a layout that you like)."
    (when (or noq
	      (and (blt-y-or-n-p "Adjust the HUD?")
		   (mess (str "Use 1/2/3 to select element, "
			      "h/j/k/l to move the selection and "
			      "escape to quit adusting."))))
      (draw-lvl lvl)
      (let ((inp (blt:read)) (messes (list "title" "messages" "stats")))
	(case inp
	  ((30 31 32) (setf pointer (* (- inp 30) 2))
	   (mess (str "++ Move the " (nth (- inp 30) messes) " ++")))
	  ((11 14) (down-to
		    (nth (+ pointer (if (evenp inp) 1 0)) *hxy*) 0))
	  (13 (up-to (nth (1+ pointer) *hxy*) 25))
	  (15 (up-to (nth pointer *hxy*) 78))
	  (41 (when (blt-y-or-n-p "All done?")
		(return-from move-hud)))))
      (move-hud lvl t)))

  (defun hud-p (&optional ?)
    "Toggle the hud-p value."
    (if ? (setf hud-p ?) hud-p)))

(defun highlight (xy ch)
  "Highlight a character in the terminal."
  (setf (blt:color) (cond ((< (report-shade) 1)
			   (blt:rgba 50 50 50))
			  ((and (eq ch #\@) (m-extra @))
			   (apply #'blt:rgba (cdr (m-extra @))))
			  (t (apply #'blt:rgba (col @ (car *vision*)))))
	(cello xy) ch
	(blt:color) (apply #'blt:rgba (car *vision*))))

(defun draw-lvl (lvl)
  "Cycle the slots in the l-struct, traversing the hash-tables in there and drawing the value elements at the k coordinates."
  (blt:clear)
  (let ((cnt (e-count @))
	(ls (list (l-seen lvl) (l-exit lvl) (l-loot lvl) (l-mobs lvl))))
    (dolist (l-struct ls (values lvl (highlight (@xy) #\@) (hud lvl)))
      (maphash
       #'(lambda (k v)
	   (if (null (e-remem v))
	       (when (= (peek >countbase k lvl) cnt)
		 (highlight k (e-ascii v)))
	       (setf (blt:color)
		     (if (= (e-count v) cnt)
			 ;; gradual color here
			 (apply #'blt:rgba
				;; blood color in extra slot (>extrabase)
				(col v (col k (or (and (listp
							(e-extra v))
						       (e-extra v))
						  (caddr *vision*))
					    'grad)))
			 (apply #'blt:rgba
				(col v (col k (cadr *vision*) 'grad))))
		     (cello k) (e-ascii v)
		     (blt:color)
		     (apply #'blt:rgba (car *vision*)))))
       l-struct))))
    

(let ((shade 1))
  
  (defun col (tile color &optional grad)
    "Manipulate color based on wall/floor and distance from @ (grad)."
    (if grad
	(let ((mh (- (round (sqrt (manhattan (@xy) tile))) 1)))
	  (modrgb (if (<= mh 0) shade (* (/ 1 mh) shade)) color))
	(modrgb (if (eq (e-ascii tile) #\.) (* .7 shade) shade)
		color)))
  
  (defun report-shade ()
    shade)
  (defun shade (n)
    (setf shade n)))


(defun cplvl (oldlvl)
  (let ((lvl (copy-l oldlvl)))
    (dolist (s (list #'l-base #'l-seen #'l-exit #'l-mobs #'l-loot) lvl)
      (loop for k being the hash-keys in (funcall s oldlvl)
	      using (hash-value v) do
		(setf (gethash k (funcall s lvl))
		      (alt-copy-structure v))))))

(defun copylvl% (oldlvl &optional changes)
  "Return a copy of oldlvl, with the hash-keys pointing to copies of the e-structs."
  (typecase changes
    (null oldlvl)
    (symbol (cplvl oldlvl))
    (function (let ((lvl (cplvl oldlvl)))
		(funcall changes lvl)
		lvl))
    (cons (let ((lvl (cplvl oldlvl)))
	    (setf (peek >base (car changes) lvl) (cdr changes))
	    lvl))
    (l changes)
    (number (cond ((zerop changes)
		   oldlvl)
		  ((camera (- (- changes (@xy))))
		   (@xy changes t)
		   oldlvl)))))

(defun alt-copy-structure (struct)
  (let ((newstruct (copy-structure struct))
	(newinv))
    (dolist (item (reverse (slot-value struct 'items)))
      (push (if item
		(copy-structure item)
		nil)
	    newinv))
    (setf (slot-value newstruct 'items) newinv)
    (when (stringp (slot-value newstruct 'descr))
      (setf (slot-value newstruct 'descr)
	    (copy-seq (slot-value struct 'descr))))
  newstruct))
