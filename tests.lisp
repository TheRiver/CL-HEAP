;;; Copyright 2009-2010 Rudolph Neeser <rudy.neeser@gmail.com>
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
;;; Some general test functionss.

(defun test-merging (test-object heap-type)
  (with-slots (heap) test-object
    (let ((heap2 (make-instance heap-type)))
      (add-all-to-heap heap2 '(4 1 5 3 6 2 7))
      (add-all-to-heap heap '(11 9 12 8 10 13))
      (let ((merged (merge-heaps heap heap2)))
	;; Ensure that all three heaps still exist.
	(loop for i from 1 to 13 do (xlunit:assert-equal i (pop-heap merged)))
      	(loop for i from 1 to 7 do (xlunit:assert-equal i (pop-heap heap2)))
	(loop for i from 8 to 13 do (xlunit:assert-equal i (pop-heap heap)))))
    (let ((max-heap (make-instance heap-type :sort-fun #'>))
	  (empty-heap (make-instance heap-type)))
      ;; Assert that a condition is thrown when the heaps differ in
      ;; key functions.
      (add-all-to-heap max-heap '(5 6 7 8))
      (add-all-to-heap heap '(1 2 3 4))
      (xlunit:assert-condition 'heap-error (merge-heaps heap max-heap))
      ;; Ensure that nothing happens when merging empty heaps.
      (merge-heaps heap empty-heap)
      (merge-heaps empty-heap heap)
      (merge-heaps empty-heap empty-heap))
    ;; Ensure that destructive merging works.
    (let ((heap2 (make-instance heap-type)))
      (add-all-to-heap heap2 '(4 1 5 3 6 2 7))
      (empty-heap heap)
      (add-all-to-heap heap '(11 9 12 8 10 13))
      (let ((merged (nmerge-heaps heap heap2)))
	;; Ensure that all three heaps still exist.
	(loop for i from 1 to 13 
	   do (xlunit:assert-equal i (pop-heap merged)))))))

(defun test-empty (test)
  (with-slots (heap) test
    (xlunit:assert-eql 0 (heap-size heap))
    (add-to-heap heap 1)
    (xlunit:assert-eql 1 (heap-size heap))
    (empty-heap heap)
    (xlunit:assert-eql 0 (heap-size heap))))

(defun test-adding (test)
  (with-slots (heap) test
    (xlunit:assert-eql 0 (heap-size heap))
    (add-all-to-heap heap '(3 7 1 8 2 6 9 4 10 5))
    (xlunit:assert-eql 10 (heap-size heap))
    (xlunit:assert-eql 1 (peep-at-heap heap))
    (xlunit:assert-eql 10 (heap-size heap))
    (loop for i from 1 to 10
       do (xlunit:assert-eql i (pop-heap heap)))))

;;;----------------------------------------------------------------

;;; This class just does some sanity testing with the priority
;;; queue. The priority queue is just a fibonacci heap, and so most of
;;; the testing will be done for that class.
(defclass priority-queue-test (xlunit:test-case)
  (queue))

(defmethod xlunit:set-up ((test priority-queue-test))
  (with-slots (queue) test
    (setf queue (make-instance 'priority-queue))))

(defmethod xlunit:tear-down ((test priority-queue-test))
  (with-slots (queue) test
    (empty-queue queue)
    (setf queue nil)))

;;;----------------------------------------------------------------

(xlunit:def-test-method order-test ((test priority-queue-test))
  "A trivial test to just ensure some sanity with the priority queue."
  (with-slots (queue) test
    (enqueue queue 'test 15)
    (enqueue queue 'this -5)
    (enqueue queue 'a 10)
    (enqueue queue 'is 5)

    (xlunit:assert-equal (dequeue queue) 'this)
    (xlunit:assert-equal (dequeue queue) 'is)
    (xlunit:assert-equal (dequeue queue) 'a)
    (xlunit:assert-equal (dequeue queue) 'test)))

;;;----------------------------------------------------------------
;;;----------------------------------------------------------------

(defclass fibonacci-test (xlunit:test-case)
  (heap
   node1
   node2))

(defmethod xlunit:set-up ((test fibonacci-test))
  (with-slots (heap node1 node2) test
    (setf heap (make-instance 'fibonacci-heap))
    (setf node1 (make-instance 'node :item 1))
    (setf node2 (make-instance 'node :item 'a))
    (concatenate-node-lists node1 (make-instance 'node :item 4))
    (concatenate-node-lists node1 (make-instance 'node :item 3))
    (concatenate-node-lists node1 (make-instance 'node :item 2))
    (concatenate-node-lists node2 (make-instance 'node :item 'd))
    (concatenate-node-lists node2 (make-instance 'node :item 'c))
    (concatenate-node-lists node2 (make-instance 'node :item 'b))))

(defmethod xlunit:tear-down ((test fibonacci-test))
  (with-slots (heap node1 node2) test
    (empty-heap heap)
    (setf heap nil
	  node1 nil
	  node2 nil)))

;;;----------------------------------------------------------------

(xlunit:def-test-method test-size ((test fibonacci-test))
  "Ensures that the heap's size makes sense as items are added and
removed."
  (with-slots (heap) test
    (loop for i from 1 upto 1000
       do
	 (add-to-heap heap (random 10000))
	 (xlunit:assert-equal (heap-size heap) i))
    (pop-heap heap)
    (xlunit:assert-equal (heap-size heap) 999)
    (loop for i from 998 downto 0
       do
	 (pop-heap heap)
	 (xlunit:assert-equal (heap-size heap) i))))

(xlunit:def-test-method test-node-lists ((test fibonacci-test))
  "Ensures that the nodes in a heap act as circular lists."
  (with-slots (node1 node2) test
    (xlunit:assert-equal (node-item node1) 1)
    (xlunit:assert-equal (node-item (node-next node1)) 2)
    (xlunit:assert-equal (node-item (node-next (node-next node1))) 3)
    (xlunit:assert-equal (node-item (node-next (node-next (node-next node1)))) 4)
    (xlunit:assert-equal (node-item (node-next (node-next (node-next (node-next node1))))) 1)
  
    (xlunit:assert-equal (node-item (node-last node1)) 4)
    (xlunit:assert-equal (node-item (node-last (node-last node1))) 3)
    (xlunit:assert-equal (node-item (node-last (node-last (node-last node1)))) 2)
    (xlunit:assert-equal (node-item (node-last (node-last (node-last (node-last node1))))) 1)

    (concatenate-node-lists node1 node2)
    (loop
       for answer in '(1 a b c d 2 3 4)
       for actual = node1 then (node-next actual)
       for i from 1 to 10
       do
	 (xlunit:assert-equal answer (node-item actual)))
    (xlunit:assert-equal 4 (node-item (node-last node1)))))

(xlunit:def-test-method test-node-deletes ((test fibonacci-test))
  "Ensures that the nodes have things properly deleted."
  (with-slots (node1) test
    (let ((second (node-next node1)))
      (delete-node second)
      (do-each-node (n node1)
	(xlunit:assert-not-eql n second))
      (setf (node-child node1) (make-instance 'node :item 10)
	    (node-rank node1) 1
	    (node-parent (node-child node1)) node1)
      (delete-node (node-child node1))
      (xlunit:assert-equal 0 (node-rank node1)))))

(xlunit:def-test-method test-iteration ((test fibonacci-test))
  "Ensures that a node list can be iterated through, while having items removed."
  (with-slots (node1) test
    (let ((items (list 1 2 3 4)))
      (do-each-node (n node1)
	(xlunit:assert-equal (pop items) (node-item n))
	(delete-node n))
      (xlunit:assert-true (null items)))))

(xlunit:def-test-method test-iteration-null ((test fibonacci-test))
  "Ensures that we do not attempt to iterate on nil, which would give
an error."
  (do-each-node (child nil)
    nil))

(xlunit:def-test-method test-delete ((test fibonacci-test))
  (with-slots (heap) test
    (let ((keys (loop for i in '(0 1 3 2 4 5)
		   collect (second (multiple-value-list (add-to-heap heap i))))))
      (pop-heap heap)
      (pop keys)
      ;; test deleting the min.
      (delete-from-heap heap (pop keys))
      (xlunit:assert-equal (peep-at-heap heap) 2)
      ;; Delete the non-min item.
      (delete-from-heap heap (pop keys))
      (xlunit:assert-equal 2 (pop-heap heap))
      (xlunit:assert-equal 4 (pop-heap heap))
      (xlunit:assert-equal 5 (pop-heap heap)))))
      

(xlunit:def-test-method test-cascade ((test fibonacci-test))
  "Ensures that cascading cuts occur as required. Builds up a tree and
goes through it, testing everything's rank. Then removes some items
and retests the ranks."
  (with-slots (heap) test
    (add-all-to-heap heap (loop for i from 1 upto 20 collect i))
    (xlunit:assert-eql 1 (pop-heap heap))
    (with-slots (root) heap
      (xlunit:assert-equal (node-rank root) 0)
      (xlunit:assert-equal (node-rank (node-next root)) 4)
      (xlunit:assert-equal (node-rank (node-next (node-next root))) 1)
      (delete-from-heap heap
			(node-child (node-next (node-child (node-next root)))))
      (delete-from-heap heap
			(node-child (node-child
				     (node-next
				      (node-child (node-next root))))))
      (delete-from-heap heap
			(node-child (node-child (node-next (node-child (node-next root))))))
      (xlunit:assert-equal (node-rank root) 0)
      (xlunit:assert-equal (node-rank (node-next root)) 1)
      (xlunit:assert-equal (node-rank (node-next (node-next root))) 0)
      (xlunit:assert-equal (node-rank (node-next (node-next (node-next root)))) 0)
      (xlunit:assert-equal (node-rank
			    (node-next (node-next (node-next (node-next root))))) 3)
      (xlunit:assert-equal (node-rank
			    (node-next (node-next
					(node-next (node-next (node-next root)))))) 1))))

(xlunit:def-test-method test-decrease-key ((test fibonacci-test))
  (with-slots (heap) test
    (let ((keys (loop for i in '(2 3 4 1 5 6)
		   collect (multiple-value-bind (val key) (add-to-heap heap i)
			     (declare (ignore val))
			     key))))
      ;; Check that decreasing a key in the root lists works.
      (decrease-key heap (pop keys) 0)
      (xlunit:assert-equal (pop-heap heap) 0)
      ;; Testing for when we have a parent
      (decrease-key heap (pop keys) 0)
      (xlunit:assert-equal (pop-heap heap) 0)
      ;; Test for errors
      (xlunit:assert-condition 'key-error (decrease-key heap (pop keys) 5))
      (xlunit:assert-equal (pop-heap heap) 1)
      ;; Test for when a non IDENTITY KEY is used.
       (let ((heap (make-instance 'fibonacci-heap :key #'(lambda (item &optional val)
							    (if val
								(setf (first item) val)
								(first item))))))
	 (let ((keys (loop for i in (list (list 4 'a) (list 1 'b) (list 3 'c))
			collect (multiple-value-bind (val key) (add-to-heap heap i)
				  (declare (ignore val))
				  key))))
	   (force-output)
	   (decrease-key heap (first keys) -1)
	   (xlunit:assert-equal (second (pop-heap heap)) 'a)))
       ;; Test that an error is thrown from decrease-key
       (let* ((heap (make-instance 'fibonacci-heap :key #'first))
	      (key  (second (multiple-value-list (add-to-heap heap '(1 a))))))
	 (xlunit:assert-condition 'key-error (decrease-key heap key 0))))))

(xlunit:def-test-method test-merge ((test fibonacci-test))
  (test-merging test 'fibonacci-heap))

(xlunit:def-test-method test-adding ((test fibonacci-test))
  (test-adding test))

(xlunit:def-test-method test-empty ((test fibonacci-test))
  (test-empty test))


;;;----------------------------------------------------------------
;;;----------------------------------------------------------------

(defclass binary-test (xlunit:test-case)
  (heap))

(defmethod xlunit:set-up ((test binary-test))
  (with-slots (heap) test
    (setf heap (make-instance 'binary-heap))))

(defmethod xlunit:tear-down ((test binary-test))
  (with-slots (heap) test
    (setf heap nil)))

;;;----------------------------------------------------------------

(xlunit:def-test-method test-positions ((test binary-test))
  "Tests the functions which determine where in the array the children
and parents are."
  ;; This tests some parent sanity checks.
  (xlunit:assert-eql 0 (parent-position 1))
  (xlunit:assert-eql 0 (parent-position 2))
  (xlunit:assert-eql 1 (parent-position 3))
  ;; Now we just check that the values of calculate child and parent positions agree.
  (loop for i from 0 to 100
     do (multiple-value-bind (left right)
	    (children-positions i)
	  (xlunit:assert-eql i (parent-position left))
	  (xlunit:assert-eql i (parent-position right)))))

(xlunit:def-test-method test-adding-popping ((test binary-test))
  (test-adding test))

(xlunit:def-test-method test-empty ((test binary-test))
  (test-empty test))
    
(xlunit:def-test-method test-merge ((test binary-test))
  (test-merging test 'binary-heap))

(xlunit:def-test-method test-decrease-key ((test binary-test))
  (with-slots (heap) test
    (let* ((to-add '(1 2 3 4 5 6))
	   (keys (loop for i in to-add
		   collect (multiple-value-bind (val key) (add-to-heap heap i)
			     (declare (ignore val))
			     key))))
      ;; Check that decreasing a key in the root lists works.
      (decrease-key heap (pop keys) -1)
      (xlunit:assert-equal (pop-heap heap) -1)
      ;; Testing for when we have a parent
      (empty-heap heap)
      (add-all-to-heap heap to-add)
      (decrease-key heap (pop keys) -1)
      (xlunit:assert-equal (pop-heap heap) -1)
      ;; Test for errors
      (empty-heap heap)
      (add-all-to-heap heap to-add)
      (xlunit:assert-condition 'key-error (decrease-key heap (pop keys) 5))
      (xlunit:assert-equal (pop-heap heap) 1)
      ;; Test for when a non IDENTITY KEY is used.
       (let* ((heap (make-instance 'binary-heap :key #'(lambda (item &optional val)
							 (if val
							     (setf (first item) val)
							     (first item))))))
	 (add-all-to-heap heap (list (list 2 'a) (list 1 'b) (list 3 'c)))
	 (xlunit:assert-equal (second (pop-heap heap)) 'b))
       ;; Test that an error is thrown from decrease-key
       (let* ((heap (make-instance 'binary-heap :key #'first))
	      (key  (second (multiple-value-list (add-to-heap heap '(1 a))))))
	 (xlunit:assert-condition 'key-error (decrease-key heap key 0))))))

(xlunit:def-test-method test-delete ((test binary-test))
  (with-slots (heap) test
    (add-all-to-heap heap '( 1 2 3 4 5 6))
    (delete-from-heap heap 0)
    (loop for i from 2 to 6 do (xlunit:assert-equal (pop-heap heap) i))))


;;;----------------------------------------------------------------
;;;----------------------------------------------------------------
