;;;; printing.lisp

(in-package #:ekzperiment)

;;; Printing -------------------------------------------------------------------------

(defun colorprint (x y string color)
  (let ((oldcolor (blt:color)))
    (setf (blt:color) (apply #'blt:rgba color))
    (blt:print x y string)
    (setf (blt:color) oldcolor)))


(let (queue)
  
  (defun canvas (&optional now-p)
    "Canvas takes a queue of lists, each having three or four elements. The first three correspond to the arguments for blt:print (x y and string), while the fourth is an optional color to use for the line."
    (dolist (lst (reverse queue))
      (if (and (= (length lst) 4)
	       (cadddr lst))
	  (colorprint (car lst) (cadr lst) (caddr lst) (cadddr lst))
	  (blt:print (car lst) (cadr lst) (caddr lst))))
    (when now-p (blt:refresh))
    (setf queue nil))
  
  (defun add-to-canvas (lst)
    "Add a list to the queue."
    (push lst queue))
  
  (defun build-canvas (&rest entries)
    "Add several lists to the queue."
    (dolist (lst entries)
      (add-to-canvas lst)))
  
  (defun release-canvas ()
    "Print the queue and leave the closure empty."
    (canvas 'now))
  
  (defun report-canvas ()
    "Report a copy of the current queue."
    (copy-tree queue)))

(defun adjust-color-screen (lvl &optional (p 0))
  "Display the adjust color screen."
  (flet ((chan () (case p ((0 3) 'red) ((1 4) 'green) ((2 5) 'blue))))
    (let ((clist (append (caddr *vision*) (cadr *vision*))))
      (blt:clear)
      (build-canvas
       (list 26 6 (str (if (> p 2) "IN MEMORY" "IN VISION") ": " (chan)))
       (list 25 12 "#.!+#" (caddr *vision*))
       (list 37 12 "#.!+#" (cadr *vision*))
       (list 20 15 "SET WORLD COLORS <h/j/k/l>"
	     (modrgb 0.7 (car *vision*))))
      (box-mess (rempar (if (zerop p)
			    (str "> " clist)
			    (str (subseq clist 0 p) " > "
				 (subseq clist p)))))
      (release-canvas)
      (case (blt:read)
	(11 (down-to p 0))
	(15 (up-to p 5))
	(14 (up-to (nth p clist) 255))
	(13 (down-to (nth p clist) 0))
	(41 (return-from adjust-color-screen)))
      (setf (caddr *vision*) (subseq clist 0 3)
	    (cadr *vision*) (subseq clist 3))
      (adjust-color-screen lvl p))))
