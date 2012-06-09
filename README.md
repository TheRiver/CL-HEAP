CL-HEAP
=======

CL-HEAP is a Common Lisp library that provides efficient priority
queue and heap implementations. Heaps are tree data structure in which
any child, c, of a parent, p, obeys the relation p R c, for some
relation R (often less-than or greater-than). Heaps can be used
directly as priority queues, and are important components of many
algorithms, like those for finding shortest paths and calculating
minimum spanning trees.

The library is simple to use. Here's an example covering most of the
priority queue functionality:

(defparameter *queue* (make-instance 'cl-heap:priority-queue))

(cl-heap:enqueue *queue* 'test 15) ; Enqueue the item with the priority of 15.
(cl-heap:enqueue *queue* 'this -5)
(cl-heap:enqueue *queue* 'a 10)
(cl-heap:enqueue *queue* 'is 5)

(cl-heap:peep-at-queue *queue*) => 'this

(cl-heap:dequeue *queue*) => 'this
(cl-heap:dequeue *queue*) => 'is
(cl-heap:dequeue *queue*) => 'a
(cl-heap:dequeue *queue*) => 'test

The library provides an implementation of an array-based binary heap -
offering O(log(n)) times for most operations - and the Fibonacci
heap. While any sequence of arbitrary operations on a Fibonacci heap
occur in amortised logarithmic time, many common operations occur in
constant and amortised constant time. See below for further
details. The Fibonacci heap should generally be your heap of choice
from this library.


Installation and Usage
----------------------

CL-HEAP depends on XLUNIT, which is used to perform unit
testing. CL-HEAP has been tested and is known to run on SBCL. If
you've tried this on other compilers and it runs successfully, please
let me know. The library can be installed using ASDF-INSTALL:

(require 'asdf-install)
(asdf-install:install 'cl-heap)

And the library can then be loaded with ASDF:

(asdf:oos 'asdf:load-op 'cl-heap)

Test Suite
----------

CL-HEAP comes with a test suite. The tests for the various classes can
be performed as follows:

(xlunit:textui-test-run (xlunit:get-suite cl-heap:fibonacci-test))
(xlunit:textui-test-run (xlunit:get-suite cl-heap:binary-test))
(xlunit:textui-test-run (xlunit:get-suite cl-heap:priority-queue-test))


The PRIORITY-QUEUE Class
------------------------

This priority queue is a container for items in which all the items
are sorted by some given priority. We implement the priority queue
using a Fibonacci heap.

MAKE-INSTANCE accepts the argument :sort-fun to provide the function
which the items' priorities are sorted by. This defaults to #'<, which
will cause the queue to always return the item of lowest priority.

(ENQUEUE queue item priority)
	 Adds the item to the queue at the given priority. Returns a
	 list containing first the item's priority, then the item
	 itself. Running time: O(1)

(DEQUEUE queue)
	 Removes the item of lowest priority from the queue. Running
	 time: amortised O(log(n))

(PEEP-AT-QUEUE queue)
	 Returns the item of lowest priority from the queue, without
	 modifying the queue. Running time: O(1)

(EMPTY-QUEUE queue)
	 Removes all items from the queue. Returns the heap. Running
	 time: O(1)

(QUEUE-SIZE queue)
	 Returns the number of items in the queue. Running time: O(1)
			    

The HEAP Class
--------------

HEAP provides functionality common to the two heap classes implement
by CL-HEAP. Each heap implementation accepts at least two arguments to
MAKE-INSTANCE: :key and :sort-fun. :sort-fun supplies the function to
be used when comparing two objects to each other, and defaults to
#'<. :key gives a function which should be first applied to the
elements in the HEAP before comparing them using the sorting
function. :key defaults to #'identity. These functions can be accessed
using HEAP-KEY and HEAP-SORTING-FUNCTION.

Both of the heap implementations obey the same interface:

(HEAP-KEY heap)
	 Returns the function passed as the :key argument to the heap.

(HEAP-SORTING-FUNCTION heap)
	 Returns the function used to sort the elements in the heap.

(HEAP-SIZE heap)
	 Returns the number of elements in the heap.

(EMPTY-HEAP heap)
	 Removes all items from the heap, and returns the heap.

(IS-EMPTY-HEAP-P heap)
	 Returns true iff the heap contains no elements.

(ADD-TO-HEAP heap item)
	 Adds the given item to the heap. Returns two values: first
	 the item itself, and then some index-key which can be used by
	 DECREASE-KEY and DELETE-KEY to identify which values should
	 be affected by these operations. See below for an example.

(ADD-ALL-TO-HEAP heap items)
	 Adds all items in the list to the heap, as if by repeated
	 calls to ADD-TO-HEAP. ADD-ALL-TO-HEAP can often be
	 implemented more efficiently, though, than by merely
	 executing consecutive calls to ADD-TO-HEAP directly. Returns
	 the heap object.

(PEEP-AT-HEAP heap)
	 Returns the minimum item in the heap, without modifying the
	 heap.

(POP-HEAP heap)
	 Removes the smallest item in the heap, and returns it.

(MERGE-HEAPS first second)
	 Returns a new heap which contains all the items in both
	 heaps. Only heaps which use the same HEAP-KEY and
	 HEAP-SORTING-FUNCTION can be merged, otherwise a HEAP-ERROR
	 is signalled.

(NMERGE-HEAPS first second)
	 Destructively creates a new heap which contains the items in
	 both heaps. Only heaps which use the same HEAP-KEY and
	 HEAP-SORTING-FUNCTION can be merged, otherwise a HEAP-ERROR
	 is signalled.

(DECREASE-KEY heap item-index value)
	 Allows you to specify an item in the heap whose value should
	 be decreased. ITEM-INDEX is the second value returned by
	 ADD-TO-HEAP, and VALUE should be smaller than the items
	 current value, or a KEY-ERROR will be signalled. Returns the
	 heap. See below for an example.

(DELETE-FROM-HEAP heap item-index)
	 Allows you to specify an arbitrary item to remove from the
	 heap. ITEM-INDEX is the second value returned by ADD-TO-HEAP.

DECREASE-KEY requires that HEAP-KEY is either the function IDENTITY,
or a function that accepts an optional second argument, which will be
the value of the new key. For instance, if the elements in the heap
are lists, and they are to be sorted by the first element in the list,
an appropriate HEAP-KEY could be:

(lambda (item &optional new-value)
    (if new-value
        (setf (first item) new-value)
	(first item)))

Of course, if DECREASE-KEY is not going to be used, then #'first
itself could simply be used as the HEAP-KEY.
	

The BINARY-HEAP Class
---------------------

BINARY-HEAP is constructed in the typical fashion, using an array. The
BINARY-HEAP MAKE-INSTANCE call accepts an extra argument, :size, which
sets the data array's initial size.

Note that DECREASE-KEY and DELETE-FROM-HEAP are not very useful
operations on this heap: while they work, the required index-keys may
be invalidated as soon as any heap operation is performed. If you do
want to make use of DELETE-FROM-HEAP or DECREASE-KEY, consider using
FIBONACCI-HEAP instead.

BINARY-HEAP provides an extra function to the above API:

(BINARY-HEAP-EXTENSION-FACTOR heap)
	 The percentage at which the space allocated for data in the
	 heap should grow at once that space has been exceeded. This
	 defaults to 50.

Here are the Big-Oh running times for the heap API, where n and m are
the sizes of various heaps.

(HEAP-SIZE heap)    	    	      => O(1)

(EMPTY-HEAP heap)		      => O(1)

(IS-EMPTY-HEAP-P heap)		      => O(1)

(ADD-TO-HEAP heap item)		      => O(log(n))

(ADD-ALL-TO-HEAP heap items)	      => O(n)

(PEEP-AT-HEAP heap)   		      => O(1)

(POP-HEAP heap)			      => O(log(n))

(MERGE-HEAPS first second)	      => O(n + m + log(n + m))

(NMERGE-HEAPS first second)	      => O(m + log(n + m))

(DECREASE-KEY heap item-index value)  => O(log(n))

(DELETE-FROM-HEAP heap item-index)    => O(log(n))


The FIBONACCI-HEAP Class
------------------------

The details of the Fibonacci heap are given in "Fibonacci Heaps and
Their Uses in Improved Network Optimization Algorithms", by Fredman
and Tarjan (see references below).

The Fibonacci heap has some interesting time constraints, and should
generally be used instead of BINARY-HEAP.

Here are the Big-Oh running times for the heap API, where n and m are
the sizes of various heaps.

(HEAP-SIZE heap)    	    	      => O(1)

(EMPTY-HEAP heap)		      => O(1)

(IS-EMPTY-HEAP-P heap)		      => O(1)

(ADD-TO-HEAP heap item)		      => O(1)

(ADD-ALL-TO-HEAP heap items)	      => O(n)

(PEEP-AT-HEAP heap)   		      => O(1)

(POP-HEAP heap)			      => amortised O(log(n))

(MERGE-HEAPS first second)	      => O(m + n)

(NMERGE-HEAPS first second)	      => O(1)

(DECREASE-KEY heap item-index value)  => amortised O(1)

(DELETE-FROM-HEAP heap item-index)    => amortised O(1), except where
		       		      	 the deleted item was the minimum
		       		         item, in which case it is
					 amortised O(log(n))


Examples of Use
---------------

A heap can be created quite simply:

(defparameter *heap* (make-instance 'cl-heap:fibonacci-heap))

This will create a heap where the elements are ordered using
#'<. Elements can be added one at a time using ADD-TO-HEAP:

(cl-heap:add-to-heap *heap* 1) 

or, in a batch.

(cl-heap:add-all-to-heap *heap* '(6 4 7 3 2 0))

The minimum item in this heap can easily by seen using PEEP-AT-HEEP,
which will not modify the heap in any way:

(cl-heap:peep-at-heap *heap*) => 0

The minimum item can be removed from the heap using POP-HEAP:

(cl-heap:pop-heap *heap*) => 0

Heaps can be used to sort items:

(let ((heap (make-instance 'cl-heap:fibonacci-heap)))
  (cl-heap:add-all-to-heap heap (loop for i from 1 upto 10 collect (random 100)))
  (loop while (not (cl-heap:is-empty-heap-p heap)) collect (cl-heap:pop-heap heap)))

=> (14 19 30 32 38 64 74 90 96 98)

A max-heap can be constructed as follows:

(defparameter *heap* (make-instance 'cl-heap:fibonacci-heap :sort-fun #'>))

And this will order things from most to least:

(let ((heap (make-instance 'cl-heap:fibonacci-heap :sort-fun #'>)))
  (cl-heap:add-all-to-heap heap (loop for i from 1 upto 10 collect (random 100)))
  (loop while (not (cl-heap:is-empty-heap-p heap)) collect (cl-heap:pop-heap heap)))

=> (69 68 64 60 37 34 30 7 6 1)  

The heaps can contain arbitrary items. For instance, a heap whose
elements are all lists can be constructed as follows:

(defparameter *heap* (make-instance 'cl-heap:fibonacci-heap :key #'first))
(cl-heap:add-to-heap *heap* (list 5 'some 'arbitrary 'data))
(cl-heap:add-to-heap *heap* (list 4 'other 'data))
(cl-heap:pop-heap *heap*) => (4 other data)

DECREASE-KEY and DELETE-FROM-HEAP can be used as follows:

(defparameter *heap* (make-instance 'cl-heap:fibonacci-heap))
(cl-heap:add-all-to-heap *heap* '(5 3 4 2 1))
(let ((item-index (second (multiple-value-list (cl-heap:add-to-heap *heap* 10)))))
     (format t "Smallest item: ~A~%" (cl-heap:peep-at-heap *heap*))
     (cl-heap:decrease-key *heap* item-index 0)
     (format t "Smallest item: ~A~%" (cl-heap:peep-at-heap *heap*)))

=> nil     
Smallest item: 1
Smallest item: 0     

License
-------

CL-HEAP is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.


References
----------
 
M. L. Fredman and R. E. Tarjan. 1987. "Fibonacci Heaps and Their Uses
in Improved Network Optimizxation Algorithms". Journal of the
Association for Computing Machinery, Vol 34, No 3. pp 596 - 615
