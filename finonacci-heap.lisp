(declaim (optimize (speed 0) (debug 3) (safety 3)))
(in-package #:cl-heap)

(defclass fibonacci-heap (heap)
  ((root :initform nil
	 :documentation "The minimum element in the tree.")
   (count :initform 0
	  :documentation "The number of items in the heap."))
  (:documentation "A heap made up of item-disjoint, heap-ordered
  trees. Has some good time constraints on various heap operations."))

(defclass node ()
  ((item :initform nil
	 :initarg :item
	 :accessor node-item)
   (parent :initform nil)
   (child :initform nil
	  :accessor node-child)
   (rank :initform 0
	 :accessor node-rank
	 :documentation "The number of children the node has.")
   (marked :initform 0)
   (next :initform nil
	 :accessor node-next)
   (last :initform nil
	 :accessor node-last)))

(defmethod initialize-instance :after ((node node) &key)
  (with-slots (next last) node
    (setf next node
	  last node)))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~a" (slot-value node 'item))))
      

      

;;;----------------------------------------------------------------
;;; Unexported functions for handling nodes.

(defgeneric concatenate-node-lists (lhs rhs)
  (:method ((lhs node) (rhs null))
    lhs)
  (:method ((lhs null) (rhs node))
    rhs)
  (:method ((lhs node) (rhs node))
    (with-slots ((lhs-next next)
		 (lhs-last last)
		 (lhs-item item)) lhs
      (with-slots ((rhs-next next)
		   (rhs-last last)
		   (rhs-item item)) rhs
	(format t "~C~CHave got ~A and ~A~%" #\tab #\tab lhs rhs)
	(psetf (node-next (node-last lhs)) rhs
	       (node-last rhs) (node-last lhs)
	       (node-next (node-last rhs)) lhs
	       (node-last lhs) (node-last rhs))))
    (format t "~C~Cconcat to ~A (other is ~A)~%" #\tab #\tab lhs rhs)
    lhs))

(defgeneric make-node-single (node)
  (:documentation "Make the node point to a list with only itself as a member.")
  (:method ((node node))
    (with-slots (next last) node
      (setf next node
	    last node))
    node))

(defgeneric delete-node (node)
  (:documentation "Deletes this node from the linked list that it
  represents, and returns the new list.")
  (:method ((node null))
    nil)
  (:method ((node node))
    (with-slots (next last) node
      (let ((result next))
	(cond
	  ((eq next node)			;Only a single thing in this list, itself.
	   nil)
	  (t
	   (psetf (node-last next) last
		  (node-next last) next
		  next node
		  last node)
	   result))))))



(defmacro do-each-node ((symbol node) &body body)
  (let ((node node)
	(last (gensym))
	(next (gensym)))
    `(loop
	with ,last = (node-last ,node)
	for ,symbol = ,node then ,next
	for ,next = (node-next ,node) then (node-next ,next)
	while (not (eq ,symbol ,last))
	do (progn
	     ,@body)
	finally (progn
		  ,@body))))


;;;--------------------
;;; Unexported functions

(defgeneric meld (one two)
  (:documentation "Joins together two fibonacci heaps."))

(defmethod meld ((heap fibonacci-heap) (item node))
  "Adds a node to the heap."
  (with-slots (root) heap
    (cond
      ((null root)
       (setf root item))
      ((compare-items heap (node-item root) (node-item item))
       (setf root (concatenate-node-lists root item)))
      (t
       (setf root (concatenate-node-lists item root)))))
  heap)

(defgeneric link (heap node-one node-two)
  (:documentation "Places node-two as a child of node-one if
  node-one's item is smaller, or vice versa.")
  (:method ((heap fibonacci-heap) (node-one node) (node-two node))
    (with-slots ((one-child child)
		 (one-item item)
		 (one-rank rank)) node-one
      (with-slots ((two-child child)
		   (two-item item)
		   (two-rank rank)) node-two
	(cond
	  ((compare-items heap one-item two-item)
	   (delete-node node-two)
	   (setf one-child (concatenate-node-lists one-child node-two))
	   (setf one-rank (max one-rank (1+ two-rank)))
	   (format t "Have linked ~A and ~A~%" node-one node-two)
	   node-one)
	  (t
	   (delete-node node-one)
	   (setf two-child (concatenate-node-lists two-child node-one))
	   (setf two-rank (max two-rank (1+ one-rank)))
	   (format t "Have linked ~A and ~A~%" node-two node-one)
	   node-two))))))

;;;----------------------------------------------------------------
;;; Exported Functions

(defmethod empty-heap ((heap fibonacci-heap))
  (with-slots (root count) heap
    (setf root nil
	  count 0))
  heap)

(defmethod is-empty-heap-p ((heap fibonacci-heap))
  (unless (slot-value heap 'root)
    t))

(defmethod heap-size ((heap fibonacci-heap))
  (slot-value heap 'count))

(defmethod add-to-heap ((heap fibonacci-heap) item)
  "Adds an item to a Fibonacci-heap. This is a constant time
oepration. Returns the item added to the heap."
  (meld heap (make-instance 'node :item item))
  (incf (slot-value heap 'count))
  item)

(defmethod add-all-to-heap ((heap fibonacci-heap) (items list))
  (with-slots (count) heap
    (loop for i in items
       do (progn
	    (meld heap (make-instance 'node :item i))
	    (incf count))))
  heap)

(defmethod peep-at-heap ((heap fibonacci-heap))
  (with-slots (root) heap
    (when root
      (node-item root))))

(defmethod pop-heap ((heap fibonacci-heap))
  (unless (is-empty-heap-p heap)
    (let ((item (peep-at-heap heap)))
      (with-slots (root) heap
	;; Delete the minimum.
	(concatenate-node-lists root (node-child root))
	(setf root (delete-node root))
	(when root
	  (let ((ranks (make-array 50 :initial-element nil))
		(min nil))
	    ;; Merge all trees of the same rank.
	    (labels ((sort-node (node)
		       (let ((position (node-rank node)))
			 (format t "Looking at ~A in position ~A~%" node position)
			 (cond
			   ((aref ranks position)
			      (format t "~CBut ~a is there already.~%"
			      #\Tab (aref ranks position))
			    (let ((new (link heap node (aref ranks position))))
			      (setf (aref ranks position) nil)
			      (sort-node new)))
			   (t
			    (format t "placing in ~A~%" position)
			    (setf (aref ranks position) node))))))
	      (do-each-node (node root)
		(format t "Loop!~%")
		(sort-node node)))
	    (format t "Ranks ~a~%" ranks)
	    (loop for tree across ranks
	       do (when (not (null tree))
		    (cond
		      ((null min)
		       (format t "init min to ~A~%" tree)
		       (setf min tree))
		      ((compare-items heap
				      (node-item min)
				      (node-item tree))
		       (format t "Setting min to ~A (1)~%" tree)
		       (setf min (concatenate-node-lists min tree)))
		      (t
		       (format t "Setting min to ~A (2)~%" tree)
		       (setf min (concatenate-node-lists tree min))))))
	    (format t "Have root ~A and min ~A~%" root min)
	    (setf root min)))
	(decf (slot-value heap 'count))
	item))))