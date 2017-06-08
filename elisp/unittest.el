;;; unittest.el --- 
;; 
;; Filename: unittest.el
;; Description: Sample ert unittests
;; Author: Matthias
;; Maintainer: Matthias (mpfeifer77@gmail.com)
;; Created: Wed Jun  7 13:56:50 2017
;; Version: 1.0
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; : 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'ert)

;; dummy methods for that will be tested

(defun method-x ( a b c )
  (cond
   ((null a) b)
   ((null b) c)
   ((null c) t)
   (t 42)))

;; define tests

(ert-deftest test-method-x-1 ()
  "Test for method-x."
  (should (equal (method-x nil 1 2) 1))
  (should (equal (method-x 1 nil 2) 2))
  (should (equal (method-x 1 2 nil) t))
  (should (equal (method-x 1 2 3) 42)))

(ert-deftest test-always-fail ()
  "This test will always fail."
  (should (equal 1 0)))

(ert-deftest test-always-succeed ()
  "This test will always succeed."
  (should-not (equal 0 1)))

;; run tests interactively

(when nil
  (call-interactively 'ert))

;; these tests should succeed

(setq test-suite (list
                  'test-method-x-1
                  'test-always-succeed))

(ert-run-test (ert-get-test 'test-method-x-1))


(ert-run-test (ert-get-test 'test-always-succeed))

(ert-deftest test-divide-by-zero ()
       (should-error (/ 1 0)
                     :type 'arith-error))

;; this test should fail
(ert-run-test (ert-get-test 'test-always-fail))

;; this test should fail

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unittest.el ends here
