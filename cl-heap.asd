;;; -*- Mode: Lisp; -*-
;;;
;;; Copyright 2009-2010, 2012 Rudolph Neeser <rudy.neeser@gmail.com>
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


(defpackage #:cl-heap-asdf
  (:use :common-lisp :asdf))

(in-package #:cl-heap-asdf)

(defsystem :cl-heap
    :description "An implementation of heap and priority queue data structures."
    :version "0.1.5"
    :author "Rudy Neeser <rudy.neeser@gmail.com>"
    :license "GPLv3"
    :serial t
    :components ((:file "package")
		 (:file "condition")
		 (:file "heap")
		 (:file "binary-heap")
		 (:file "fibonacci-heap")
		 (:file "priority-queue")))

