;;; Copyright 2009-2010 Rudolph Neeser <rudy.neeser@gmail.com>.
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
;;; Some useful definitions: Through the use of SORT-FUN, each HEAP
;;; can be used as either a min or a max heap, or even as something
;;; abstract by defining an arbitrary total relation on the objects
;;; being inserted into the heap. For any relation R and two items x
;;; and y for which x R y is a true, we will call x the lesser of the
;;; two items.
;;; --------------------------------------------------------------------

(defclass heap ()
  ((sort-fun :initform #'<
	     :reader heap-sorting-function
	     :initarg :sort-fun
	     :documentation "The function used to apply an order to
   elements in the heap.")
   (key :initform #'identity
	:reader heap-key
	:initarg :key
	:documentation "A function used to obtain the elements which
	should be compared to give an order to the items in the
	heap."))
  (:documentation "The base type of the two HEAP implementations."))

;;;--------------------------------------------------------------------
;;; Exported functions.

(defgeneric heap-size (heap)
  (:documentation "Returns the number of elements in the heap."))

(defgeneric add-to-heap (heap item)
  (:documentation "Inserts an item into the given HEAP data
  structure. Returns two values: first, the item added, and then an
  index-key which can be used to identify the item for DECREASE-KEY
  and DELETE-FROM-HEAP operations."))


(defgeneric add-all-to-heap (heap items)
  (:documentation "Adds a list of items to a HEAP. This can typically
  be done more efficiently than by using repeated calls to
  ADD-TO-HEAP, since the heap order only needs to be reinstated after
  all of the items have been inserted, rather than maintained through
  each operation. Returns the heap object."))

(defgeneric peep-at-heap (heap)
  (:documentation "Returns the heap's root, without modifying the
  heap. Returns nil if the heap is empty."))

(defgeneric pop-heap (heap)
  (:documentation "Removes the top element from the heap and returns
  it."))

(defgeneric empty-heap (heap)
  (:documentation "Removes all contents from the given heap. Returns the heap."))

(defgeneric is-empty-heap-p (heap)
  (:documentation "Returns true iff the given heap is empty."))

(defgeneric merge-heaps (first second)
  (:documentation "Returns a new heap that is the merged copy of those
  given here. Can only merge heaps which use the same key and sorting
  function. The two arguments are nt modified by this function."))

(defgeneric nmerge-heaps (first second)
  (:documentation "Destructively updates the arguments to contain the
  merge of both heaps, which is then returned. Can only merge heaps
  which use the same key and sorting function."))

(defgeneric decrease-key (heap item-index value)
  (:documentation "Decreases the value of the item represented by
  ITEM-INDEX. ITEM-INDEX is the index returned by ADD-TO-HEAP for a
  particular item. VALUE is the item's new value, and this must be
  \"less\" than its old value. Returns the heap."))


(defgeneric delete-from-heap (heap item-index)
  (:documentation "Removes the item from the heap represented by the
  ITEM-INDEX. This index is returned as the second value of
  ADD-TO-HEAP. Returns the heap."))

;;;--------------------------------------------------------------------
;;; Unexported functions

(defgeneric compare-items (heap parent child)
  (:documentation "Compares two items, using the HEAP's SORT-FUN and
  KEY functions.")
  (:method ((heap heap) parent child)
    (with-slots (sort-fun key) heap
      (funcall sort-fun (funcall key parent) (funcall key child)))))

;;;--------------------------------------------------------------------
;;; Various implementation details

(defmethod print-object ((heap heap) stream)
  (print-unreadable-object (heap stream :type t :identity t)
    (with-slots (key sort-fun) heap
      (format stream "Size: ~A"	 (heap-size heap)))))

