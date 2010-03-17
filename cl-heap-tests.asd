;;; -*- Mode: Lisp; -*-
;;;
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


(defpackage #:cl-heap-tests-asdf
  (:use :common-lisp :asdf))

(in-package #:cl-heap-tests-asdf)

(defsystem :cl-heap-tests
    :description "Tests for the CL-HEAP package, an implementation of
    heap and priority queue data structures."
    :version "0.1.3"
    :author "Rudy Neeser <rudy.neeser@gmail.com>"
    :license "GPLv3"
    :depends-on (:xlunit :cl-heap)
    :serial t
    :components ((:file "tests")))

