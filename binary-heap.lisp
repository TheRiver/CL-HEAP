(declaim (optimize (debug 3) (safety 3) (speed 0)))
;;; Copyright 2009 Rudolph Neeser <rudy.neeser@gmail.com>.
;;; 
;;; This file is part of CL-HEAP
;;; 
;;; CL-HEAP is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; CL-HEAP is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with CL-HEAP.  If not, see <http://www.gnu.org/licenses/>.

;;;----------------------------------------------------------------

(in-package #:cl-heap)

;;;--------------------------------------------------------------------

(defclass binary-heap (heap)
  ((data :initform nil
	 :documentation "The heap's stored data.")
   (extension-factor :initform 50
		     :accessor binary-heap-extension-factor
		     :documentation "The percentage at which the space
		     allocated for data in the heap should grow at
		     once that space has been exceeded."))
  (:documentation "An implementation of a classic binary heap. This
   uses the standard array-based implementation."))

(defmethod initialize-instance :after ((heap binary-heap) &key (size 50))
  (with-slots (data) heap
    (setf data (make-array size :adjustable t :fill-pointer 0))))

;;;--------------------------------------------------------------------
;;; Unexported functions

(declaim (inline children-positions))
(defun children-positions (position)
  (declare (type fixnum position))
  (check-type position (integer 0 *))
  (values (1+ (* position 2))
	  (+ 2 (* position 2))))

(declaim (inline parent-position))
(defun parent-position (position)
  (declare (type fixnum position))
  (check-type position (integer 0 *))
  (values (floor (/ (1- position) 2))))

(defgeneric percolate-down (heap position)
  (:documentation "Used to move a value in the DATA array of a
  BINARY-HEAP down the parent-child relationship hierarchy, and so
  preserve heap-ordering.")
  (:method ((heap binary-heap) (position fixnum))
    (with-slots (data) heap
      (labels ((choose-one (lhs rhs)
		 ;; Of two nodes, returns the "least"
		 (cond
		   ((>= rhs (length data))
		    lhs)
		   ((compare-items heap
				   (aref data lhs)
				   (aref data rhs))
		    lhs)
		   (t
		    rhs))))
	(multiple-value-bind (left-child right-child)
	    (children-positions position)
	  (when (and (< (children-positions position)
			(length data))
		     (not
		      (and (compare-items heap
					  (aref data position)
					  (aref data left-child))
			   (or
			    (>= (1+ (children-positions position)) (length data))
			    (compare-items heap
					   (aref data position)
					   (aref data right-child))))))
	    (let ((chosen (choose-one left-child right-child)))
	      (rotatef (aref data position)
		       (aref data chosen))
	      (percolate-down heap chosen))))))))

(defgeneric percolate-up (heap position)
  (:method ((heap binary-heap) (position fixnum))
    (with-slots (data) heap
      (cond
	((and (/= position 0)
	      (not (compare-items heap
				  (aref data
					(parent-position position))
				  (aref data position))))
	 (rotatef (aref data position)
		  (aref data (parent-position position)))
	 (percolate-up heap (parent-position position)))
	(t
	 position)))))

	      
;;;--------------------------------------------------------------------
;;; Exported functions

(defmethod heap-size ((heap binary-heap))
  (length (slot-value heap 'data)))

(defmethod empty-heap ((heap binary-heap))
  "Clears the heap. A constant time operation."
  (with-slots (data) heap
    (setf (fill-pointer data) 0))
  heap)

(defmethod is-empty-heap-p ((heap binary-heap))
  (with-slots (data) heap
    (= (length data) 0)))

(defmethod peep-at-heap ((heap binary-heap))
  "Returns the minimum object of the heap, without updating the
heap. This is an O(1) operation."
  (with-slots (data) heap
    (when (plusp (length data))
      (aref data 0))))

(defmethod add-to-heap ((heap binary-heap) item)
  "Inserts an item into a BINARY-HEAP. This is O(log(n)), n being the
number of items in the heap."
  (with-slots (data
	       (factor extension-factor)) heap
    (vector-push-extend item data (ceiling (* (/ factor 100)  (array-total-size data))))
    (values item (percolate-up heap (1- (length data))))))

(defmethod pop-heap ((heap binary-heap))
  "Removes the minimum item from the heap. This is an O(log(n))
operation, where n is the number of items in the heap."
  (with-slots (data) heap
    (when (plusp (length data))
      (let ((top (aref data 0)))
	;; Place the last element into the root
	(setf (aref data 0) (aref data (1- (length data))))
	(decf (fill-pointer data))
	(percolate-down heap 0)
	top))))
	  
(defmethod add-all-to-heap ((heap binary-heap) (items list))
  "Adds all the items in the list into the BINARY-HEAP. This is a
O(n + log(n)) = O(n) operation. Returns the BINARY-HEAP."
  (with-slots (data
	       (factor extension-factor)) heap
    ;; Add all items, which is linear time since no sorting occurs here.
    (loop for item in items
       do (vector-push-extend item data (ceiling (* (/ factor 100) (array-total-size data)))))
    (loop for position from (parent-position (1- (length data))) downto 0
       do (percolate-down heap position)))
  heap)


(defmethod merge-heaps ((first binary-heap) (second binary-heap))
  "Non-destructively merges two BINARY-HEAPs. This is an O(n + m +
log(n + m)) operation, n and m being the zies of the two heaps."
  (unless (and (eq (heap-sorting-function first) (heap-sorting-function second))
	       (eq (heap-key first) (heap-key second)))
    (error 'heap-error :message
	   "The two heaps do not using the same sorting function or key."))
  (with-slots ((first-data data)) first
    (with-slots ((second-data data)) second
      (let* ((length (+ (length first-data) (length second-data)))
	      (result (make-instance 'binary-heap
				     :size length
				     :sort-fun (heap-sorting-function first)
				     :key (heap-key first))))
	(with-slots (data) result
	  (setf (fill-pointer data) length
		(subseq data 0 (length first-data)) first-data
		(subseq data (length first-data)) second-data)
	  (when (plusp (length data))
	    (loop for position from (parent-position (1- (length data))) downto 0
	       do (percolate-down result position))))
	result))))

(defmethod nmerge-heaps ((first binary-heap) (second binary-heap))
  "Destructively merges two BINARY-HEAPs, and returns the
result. Where n and m are the sizes of each queue, this is an order
O(m + log(n + m) operation, unless an array resize is required, for
which it becomes O(n + m + log(n + m))."
  (unless (and (eq (heap-sorting-function first) (heap-sorting-function second))
	       (eq (heap-key first) (heap-key second)))
    (error 'heap-error
	   :message "The two heaps do not using the same sorting function or key."))
  (with-slots ((first-data data)) first
    (with-slots ((second-data data)) second
      (let ((original-length (length first-data))
	    (length (+ (length first-data) (length second-data))))
	(when (< (length first-data) length)
	  (adjust-array first-data length))
	(setf (fill-pointer first-data) length
	      (subseq first-data 0 original-length) first-data
	      (subseq first-data original-length) second-data)
	(when (plusp (length first-data))
	  (loop for position from (parent-position (1- (length first-data))) downto 0
	     do (percolate-down first position))))
      first)))

(defmethod decrease-key ((heap binary-heap) (item-index fixnum) value)
  "Deceases the key of the item pointed to by ITEM-INDEX. The index is
  returned as the second value of ADD-TO-HEAP. The value of the item
  at the index is changed to VALUE, which should be less than its old
  value. This operation is O(log(n)), with n being the number of items
  in the heap. Note that using a binary heap is not ideal for this
  operation, since the item pointed to by any given index can be
  changed by performing any heap operation. This function is provided
  mostly for completeness."
  (check-type item-index (integer 0 *))
  (with-slots (data sort-fun key) heap
    (unless (funcall sort-fun value (funcall key (aref data item-index)))
      (error 'key-error
	     :message
	     (format nil
		     "The given value (~a) must be less than the current value (~a)."
		     value (funcall key (aref data item-index)))))
    (cond
      ((not (eq (heap-key heap) #'identity))
       (handler-case (funcall (heap-key heap)
			      (aref data item-index)
			      value)
	 (error (e)
	   (declare (ignore e))
	   (error 'key-error))))
      (t
       (setf (aref data item-index) value)))
    (percolate-up heap item-index))
  heap)

(defmethod delete-from-heap ((heap binary-heap) (item-index fixnum))
  "Deltes an item from the heap. ITEM-INDEX is an index representing
  the value to remove, and is the second value returned from
  ADD-TO-HEAP. Note that running most HEAP functions can modify which
  value is pointed to by ITEM-INDEX, so this function is given mostly
  for completeness. Use a Fibonacci heap if this functionality is
  important to you. This operation completes in O(log(n)) time, where
  n is the number of items in the heap."
  (check-type item-index (integer 0 *))
  (with-slots (data) heap
    (when (plusp (length data))
      ;; Place the last element into this position
      (setf (aref data item-index) (aref data (1- (length data))))
      (decf (fill-pointer data))
      (percolate-down heap item-index)
      heap)))
			 
    
;;;--------------------------------------------------------------------