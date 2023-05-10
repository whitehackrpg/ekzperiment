;;;; astar.lisp

(in-package #:ekzperiment)

;;;; A* Pathfinding
; -----------------------------------------------------------

(defun backtrace-path (node)
  "Make a list out of node parents."
  (unless (null (node-parent node))
    (cons node (backtrace-path (node-parent node)))))

(defun cost (node)
  "A* movement costs (diagonal 1.4)"
  (if (or (zerop (realpart node))
	  (zerop (imagpart node)))
      10
      14))

(defun manhattan (node target)
  "Heuristic distance. Square root can be omitted because the return value will always be compared to another manhattan return value."
  (+ (expt (- (realpart node) (realpart target)) 2)
     (expt (- (imagpart node) (imagpart target)) 2)))

(defun a-star (start target lvl)
  "A standard A* routine with hash-tables for faster lookup."
  (do* ((open (make-hash-table))
	(o-lst (list (make-node :xy start)))
	(closed (make-hash-table))
	(current (setf (gethash start open) (car o-lst)) (pop o-lst)))
       ((null o-lst))
    (remhash (node-xy current) open)
    (setf (gethash (node-xy current) closed) current)
    (if (= (node-xy current) target)
	(return (nreverse (backtrace-path current)))
	(dolist (child *surr*)
	  (let* ((child-xy (+ child (node-xy current))) 
		 (child-g (+ (node-g current) (cost child)))
		 (child-h (manhattan child-xy target))
		 (child-f (+ child-g child-h))
		 (already (gethash child-xy open))
		 (on-closed (gethash child-xy closed)))
	    (unless (or on-closed
			(null (peek >base child-xy lvl)) ; out of bounds
			(or-eq (peek >asciibase child-xy lvl (make-e))
			       #\# #\+)
			(and (= target (@xy)) ; mob searches path to @
			     (peek >mobs child-xy lvl))) ; finds mob
	      (if (null already)
		  (let ((new-node (make-node :xy child-xy :parent current
				   :f child-f :g child-g :h child-h)))
		    (setf (gethash child-xy open) new-node
			  o-lst (merge 'list (list new-node) o-lst
				       #'< :key #'node-f)))
		  (when (> (node-g already) child-g)
		    (setf (node-g already) child-g
			  (node-parent already) current
			  (node-f already) (+ child-g
					      (node-h already)))))))))))
