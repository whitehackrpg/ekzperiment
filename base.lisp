;;;; base.lisp

(in-package #:ekzperiment)


#| List of things to fix:

+ Implement recipes
+ Limit the amount of ammo a gun can hold
+ Include an end item to bring back.
+ Monsters dropping stuff.
+ Descriptions of mobs and stuff on the ground.
+ A list of keys and instructions. <-- I forget what this is!
+ A diary or camera (or something homing in on a known exit).
+ Missions.
+ Story elements.
+ Optimize a-star?
! An enclosing trading system.
! An enclosing travel system. |#

;;; Structs, utilities & macros -----------------------------------------------------------------------

#+win32
(progn (cffi:define-foreign-library blt:bearlibterminal
	 (t "./BearLibTerminal.dll"))
       (cffi:use-foreign-library blt:bearlibterminal))

(defstruct e
  "The entity structure. Is used for tiles, items and mobs, although the latter two have their own, specialized structures inheriting this one."
  (ascii #\#) ; character, like #\!
  (count 0)   ; count, used a lot for field of view
  (items nil) ; inventory, what somethings holds (loot, ammo etc.)
  (fsymb nil) ; a symbol with a function slot defined elsewhere
  (units 10)  ; health, damage, efficiency ...
  descr       ; info, usually a format string
  extra       ; something, can be anything
  (remem t))  ; memory, used mostly for field of view

(defstruct (i (:include e))
  "Items need a few extra slots."
  model       ; item model
  cterm       ; count terminology
  uterm       ; units terminology
  craft)      ; a combination pattern used for combos

(defstruct (m (:include e))
  "As do mobs."
  aware       ; the mob's perception
  (taint 5)       ;
  smart       ; the mob's intelligence
  words)      ; a list of things the mob might say)

(defstruct l
  "The level structure."
  (dimension (cons 0 0) :type cons)          ; '(xdim . ydim)
  (number 0             :type integer)       ; level number
  (base nil             :type hash-table)    ; all base tiles
  (seen nil             :type hash-table)    ; seen tiles
  (exit nil             :type hash-table)    ; exits to other levels
  (mobs nil             :type hash-table)    ; the mobs at their coords
  (loot nil             :type hash-table))   ; the loot at certain coords

(defstruct node
  "Node data for the a-star path-finding algorithm."
  xy parent (f 0) (g 0) (h 0))

(defvar *font-size* 36)

(defparameter *font* (asdf:system-relative-pathname "ekzperiment" "fonts/fixedsys-excelsior-301.ttf"))

(defparameter *tdims* (cons 70 20)
  "The dimensions of the terminal.")

(defparameter *mdims* (list 25 85 25 85)
  "The minimum and maximum for the x and y number of map tiles, in that order. Note that the axes start at zero and go to one less than cadr and cadddr.")

(defparameter *hxy* (list 44 0 44 9 44 2 44 5)
  "HUD positions in pairs of x and y values. Adjust in game.")

(defparameter *vision* (list (list 200 200 200) ; text, standouts
			     (list 117 117 177) ; remembered tiles
			     (list 234 146 221)); tiles in fov
  "The default color scheme. Adjust in game.")

(defparameter *surr* (list #C( 0 -1) #C(0  1) #C(-1  0) #C(1  0)
			   #C(-1 -1) #C(1 -1) #C(-1  1) #C(1  1))
  "Complex modifiers to get the coords around a coord.")

(defparameter *messages* (make-list 20 :initial-element "")
  "The list of in-game messages.")

(defparameter *fovhash* nil ; initialized in main
  "The hash of complex modifications to the character location, used to shoot rays.")

(defparameter *ci* 0 "Current inventory column.")

(defparameter *li* (list 0 0) "Current inventory rows.")

(defparameter *loud-sound* nil)

(defparameter *combinables*
  (list (list 'clip 'clip 'clip)
	(list 'potion 'potion 'potion)
	(list 'clip 'potion 'bomb)
	(list 'armor 'potion 'armor)))


(defun str (&rest components)
  "Make a string of just about any components."
  (with-output-to-string (stream)
    (dolist (comp components)
      (princ comp stream))))

(defun mstr (test string)
  "Test and return a copy of string with either . or s. at the end."
  (str string (if test "s." ".")))

(defun symb (&rest components)
  "Make a symbol of just about any components."
  (values (intern (apply #'str components))))

(defun find-combo (item1 item2)
  (unless (or-eq nil item1 item2)
  (dolist (combo *combinables*)
    (when (and (member (i-fsymb item1) combo)
	       (member (i-fsymb item2) combo))
      (return-from find-combo (caddr combo))))))

(defmacro peek (a b &optional c d)
  "Peek somewhere in the game data. Use it like a gethash, but optionally concatenate > with an e-slot name, an l-slot name, or both, in that order, and place it before the regular args. For example, (peek >asciibase xy lvl) expands to (e-ascii (gethash xy (l-base lvl))), while (peek >loot xy lvl) expands to (gethash xy (l-loot lvl))."
  (let ((symbol (when (symbolp a) (symbol-name a))))
    (if (and symbol (string= (subseq symbol 0 1) ">"))
	(let ((s (subseq symbol 1)))
	  (case (length s)
	    (9 `(,(symb 'e- (subseq s 0 5))
		 (gethash ,b (,(symb 'l- (subseq s 5)) ,c) ,d)))
	    (5 `(,(symb 'e- (subseq s 0)) (gethash ,b ,c ,d)))
	    (t `(gethash ,b (,(symb 'l- (subseq s 0)) ,c) ,d))))
	`(gethash ,a ,b ,c))))

 
(defun modrgb (m rgb)
  "Modify a list of red, green and blue color values by a factor from 0 to 1. I left the asserts because bugs can be hard to find here."
  (assert (within m 0 1) (m) "The modifier ~A isn't between 0 and 1." m)
  (assert (every #'(lambda (n) (within n 0 255)) rgb) (rgb) "The RGB list ~A contains one or more values that isn't between 0 and 255." rgb)
  (mapcar #'(lambda (n) (round (* m n))) rgb))

(defun mq (&rest args)
  "Check if multiple things are eq."
  (every #'(lambda (n) (eq n (car args))) (cdr args)))

(defun strdc (arg)
  "This variant of string-downcase returns an empty string on a null arg (the original returns nil surrounded by double quotes)."
  (if (null arg)
      ""
      (string-downcase arg)))

(defun shuffle (list)
  "Shuffle the elements of a list."
  (labels ((shuffl (lst num)
	     (unless (null lst)
	       (let ((el (nth (random num) lst)))
		 (cons el (shuffl (remove el lst :count 1) (1- num)))))))
    (shuffl list (length list))))

(defun find-middle (a b)
  (let ((diff (- b a)))
    (complex (+ (realpart a) (floor (realpart diff) 2))
	     (+ (imagpart a) (floor (imagpart diff) 2)))))


(defun expand (list)
  (let (newlist)
    (labels ((pusher (e n) (unless (zerop n)
			     (push e newlist) (pusher e (1- n)))))
    (dolist (entry list (reverse newlist))
      (if (atom entry)
	  (push entry newlist)
	  (pusher (car entry) (cadr entry)))))))

(defun rel (list)
  "Pick random element from list."
  (when (consp list)
    (nth (random (length list)) list)))

(defun or-eq (thing &rest args)
  "Brief version of some. See if thing is eq to one or more of args."
  (some #'(lambda (n) (eq thing n)) args))

(defun rpart (n m)
  "Randomize a part (m) of an n amount."
  (+ (random m) (- n m)))

(defun mkline (n c)
  "Shorthand for make-string with an initial element."
  (make-string n :initial-element c))

(defun box-mess (string)
  "Print a one line message in a box in the middle of the screen."
  (let ((len (length string)))
    (if (> len 70)
	(long-box-mess string)
	(let* ((x (- (floor (/ (- (car *tdims*) len) 2)) 2))
	       (y 8))
	  (blt:draw-box (- x 2) (1- y) (+ 6 len) 5)
	  (blt:print (1+ x) (1+ y) string)))))

(defun long-box-mess (string)
  "Print a long message in the middle of the screen."
  (blt:clear)
  (let* ((message (linebreaker 66 string))
	 (len (length message))
	 (y (+ 5 (floor (- 8 len) 2))))
    (blt:draw-box 5 y 70 (+ len 4))
    (dotimes (line len (blt:refresh))
      (blt:print 8 (+ y 2 line) (apply #'str (nth line message))))))

(defun linebreaker (n string)
  "Linebreak a longer string into lines no longer than n."
  (labels ((lb (lst line result count)
	     (let* ((word (concatenate 'string (car lst) " "))
		    (len (length word)))
	       (cond ((null lst) (reverse (cons line result)))
		     ((> (+ count len) n)
		      (lb (cdr lst) (list word) (cons line result) len))
		     (t (lb (cdr lst) (append line (list word)) result
			    (+ count len)))))))
    (lb (uiop:split-string string :separator " ") nil nil 0)))

(defun blt-y-or-n-p (string)
  "Like y-or-n-p, but in the BearLib Terminal."
  (box-mess string)
  (blt:refresh)
  (case (blt:read)
    (28 t) ;y
    (17) ; n
    (t (blt-y-or-n-p string))))

(defun within (a b c)
  "Is a within b and c?"
  (and (>= a b)
       (<= a c)))

(defun cwithin (c y z m n)
  "Is the realpart of c within y and z, and the imagpart of c within m and n?"
  (and (within (realpart c) y z)
       (within (imagpart c) m n)))

(defun toward (x y)
  "Return next value going from x to y."
  (cond ((= x y) x)
	((< x y) (1+ x))
	(t (1- x))))

(defun ctoward (c1 c2)
  "Return a complex modification to move c1 toward c2."
  (- (complex (toward (realpart c1) (realpart c2))
	      (toward (imagpart c1) (imagpart c2)))
     c1))

(defun rdim (dimx dimy)
  "Return random coord."
  (complex (random dimx) (random dimy)))

(defmacro levels (n)
  "Shortcut for (gethash n levels)"
  `(gethash ,n levels))

(defmacro cello (n)
  "Expand to (blt:cell-char (realpart n) (imagpart n))."
  `(cell (offset ,n)))

(defmacro cell (n)
  "Helper macro for cello."
  `(blt:cell-char (realpart ,n) (imagpart ,n)))

(defmacro up-to (val max &optional body)
  "Increase val unless it would bring it above max. Destructive."
  `(if (> (1+ ,val) ,max)
       (values ,max ,body)
       (incf ,val)))

(defmacro down-to (val min &optional body)
  "Decrease a val unless it would bring it below min. Desctructive."
  `(if (< (1- ,val) ,min)
       (values ,min ,body)
       (decf ,val)))

(defun rempar (string)
  "Return a string without parentheses."
  (remove #\( (remove #\) string)))
