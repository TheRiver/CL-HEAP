;;; Copyright 2009 Rudolph Neeser <rudy.neeser@gmail.com>
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

;;;----------------------------------------------------------------

(defclass priority-queue ()
  ((heap))
  (:documentation "A simple priority queue implementaiton, based on a
  Fibonacci heap."))

(defmethod initialize-instance :after ((queue priority-queue) &key (sort-fun #'<))
  (with-slots (heap) queue
    (setf heap (make-instance 'fibonacci-heap :key #'first :sort-fun sort-fun))))

;;;----------------------------------------------------------------

(declaim (inline enqueue))
(defgeneric enqueue (queue item priority)
  (:documentation "Adds an item with a given priority on to the
  queue. Returns a list containing first the priority, then the
  item. This is a constant time operation.")
  (:method ((queue priority-queue) item priority)
    (with-slots (heap) queue
      (values (add-to-heap heap (list priority item))))))

(declaim (inline dequeue))
(defgeneric dequeue (queue)
  (:documentation "Removes an element from the front of the queue and
  returns it. This is an amortised O(log(n)) operation.")
  (:method (queue)
    (with-slots (heap) queue
      (second (pop-heap heap)))))

(declaim (inline peep-at-queue))
(defgeneric peep-at-queue (queue)
  (:documentation "Returns the element at the front of the queue,
  without modifying the queue. This is a constant time operation.")
  (:method ((queue priority-queue))
    (with-slots (heap) queue
      (second (peep-at-heap heap)))))

(declaim (inline empty-queue))
(defgeneric empty-queue (queue)
  (:documentation "Removes all items from the queue. This is a constant time operation.")
  (:method ((queue priority-queue))
    (with-slots (heap) queue
      (empty-heap heap))))

(declaim (inline queue-size))
(defgeneric queue-size (queue)
  (:documentation "Returns the number of elements in the queue.")
  (:method ((queue priority-queue))
    (with-slots (heap) queue
      (heap-size heap))))