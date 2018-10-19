;;
;;
;; Set utilities for COGS 543
;;
;; Umut Ozge -- tumuum@gmail.com
;;

; (defpackage :setutils 
;   (:use :common-lisp)
;   (:nicknames :su)
;   (:export :setp :random-pick :generate-tuples))
; 
; (in-package setutils)

(defparameter *test* '(1 2 3 4 5 6 7 8 9 10))

(defun setp (lst)
  "test for sethood"
  (or (null lst) 
      (and (not (member (car lst) (cdr lst))) 
           (setp (cdr lst)))))

(defun random-pick (set)
  "randomly pick an element from a set"
  (nth (random (length set)) set))

(defun pick-a-subset-r (set &optional (size (random (+ (length set) 1))))
  "recursive subset generator -- picks a random subset if size argument is missing"
  (if (zerop size)
    nil
    (let ((pick (random-pick set)))
      (cons pick (pick-a-subset-r (remove pick set) (- size 1))))))

(defun pick-a-subset-i (set &optional (size (random (+ (length set) 1))))
  "iterative subset generator -- picks a random subset if size argument is missing"
    (do ((accu nil (cons (random-pick set) accu)))
      ((eql (length accu) size) accu)
      (setf set (remove (car accu) set))))

(defun pick-a-subset (set &optional (size (random (+ (length set) 1))))
  "a funny function that randomly decides to use an iterative or a recursive function to pick a subset from a given set -- if size is missing, picks a random subset"
  (funcall (intern (concatenate 'string "PICK-A-SUBSET-" (random-pick '("I" "R")))) set size))




;;
;; Cartesian product of a collection of sets
;; Version 1
;;
;; This set does all in one full sweep; recurses over the collection (set-of-sets)
;; and takes the product on the fly while recursing.
;; Nested MAPCAR's might be a little cryptic, if you're not used to them.

(defun cartesian-product (set-of-sets &optional (accu '(nil)))
  "computes the cartesian product of the sets in set-of-sets"
  (if (endp set-of-sets)
    accu
    (cartesian-product
      (cdr set-of-sets)
      (reduce 'union (mapcar
                      #'(lambda (x)
                          (mapcar
                            #'(lambda (y) (append x (list y)))
                            (car set-of-sets)))
                      accu)))))




;;
;; Cartesian product of a collection of sets
;; Version 2
;;
;; First a "procedural" flavored binary cartestion product taker  (CARTESIAN-PRODUCT-BINARY);
;; then REDUCE will be enough to apply it to 
;; a set of sets.

(defun  cartesian-product-binary (set1 set2)
  "compute the cartesian product of the given sets -- the first set may be a set of tuples rather than atoms"
  (let ((accu nil))
    (dolist (x set1 accu)
      (dolist (y set2)
        (setf accu (cons
                     (append (if (listp x) x (list x)) (list y))
                     accu))))))

(defun cartesian-product-v2 (set-of-sets)
  (reduce #'cartesian-product-binary set-of-sets))




(defun generate-tuples (baseset n)
  "generates a random set of n-tuples from baseset"
  (cond ((zerop n) nil)
        ((eql 1 n) (pick-a-subset baseset))
        (t
          (pick-a-subset
            (cartesian-product
              (make-list n :initial-element baseset))))))
