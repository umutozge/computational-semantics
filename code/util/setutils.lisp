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

(defparameter *test* '(1 2 3 4 5))

(defun setp (lst)
  "test for sethood"
  (or (null lst) 
      (and (not (member (car lst) (cdr lst))) 
           (setp (cdr lst)))))

(defun singletonp (lst)
  "test for singleton sets"
  (and (consp lst) (null (cdr lst))))

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
  "a funny function that randomly decides to use an iterative or a recursive 
  function to pick a subset from a given set -- if size is missing, picks a random subset"
  (funcall (random-pick '(pick-a-subset-i pick-a-subset-r)) set size))
;   (funcall (intern (concatenate 'string "PICK-A-SUBSET-" (random-pick '("I" "R")))) set size))

;;
;; Cartesian product of a collection of sets
;; Version 1
;;
;; This set does all in one full sweep; recurses over the collection (set-of-sets)
;; and takes the product on the fly while recursing.
;; Nested MAPCAR's might be a little cryptic, if you're not used to them.

(defun cartesian-product (set-of-sets &optional accu)
  "computes the cartesian product of the sets in set-of-sets"
  (cond ((endp set-of-sets) accu)
        ((null accu) (cartesian-product
                       (cdr set-of-sets)
                       (mapcar 
                         #'(lambda (x) (list x))
                         (car set-of-sets))))
        (t (cartesian-product
             (cdr set-of-sets)
             (reduce 'union (mapcar
                              #'(lambda (x)
                                  (mapcar
                                    #'(lambda (y) (append x (list y)))
                                    (car set-of-sets)))
                              accu))))))
;;
;; Cartesian product of a collection of sets
;; Version 2
;;
;; First a "procedural" flavored binary cartestion product taker  (CARTESIAN-PRODUCT-BINARY);
;; then REDUCE will be enough to apply it to 
;; a set of sets.

(defun cartesian-product-binary (set1 set2)
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

;;
;; Functions-to-sets: turning set representations to functions
;;

;; First some utilities:


(defun binary-relationp (relation)
  "superficially (only first tuple) checks if relation is a binary relation -- a set of ordered pairs"
  (let ((first-tuple (car relation)))
    (and first-tuple (singletonp (cdr first-tuple)))))
  

(defun collect-image (item relation &optional accu)
  "collects the elements in the image of an object to a list.
  For an object x_0 and relation R, the image of x_0 is 
  {(x_1,...,x_n) | (x_0,x_1,...,x_n) is in R, for n > 0}.
  Note that ASSOC is not helpful, as it gets only the first match.
  "
  (if (null relation)
    accu
    (let ((pick-item (if (binary-relationp relation) ; if you have a bin rel. make sure that what you 
                       #'cadar                         ; collect is a proper set rather than a set of singletons
                       #'cdar)))
      (collect-image
        item 
        (cdr relation)
        (if (equal (caar relation) item)
          (cons (funcall pick-item relation) accu)
          accu)))))

;; A base case for turning sets to functions is that of a set, namely 
;; the characteristic function of a set

(defun get-char-function (input-set) 
  "return the characteristic function of the input-set"
  #'(lambda (x) (null (null (member x input-set)))))


;; Now the general case; our input is a set of tuples, and the output
;; is a curried function representing the set.

(defun set-to-function (set-of-tuples)
  "constructs a curried function from the set representation of a first-order relation"
    (if (consp (car set-of-tuples))
       #'(lambda (x)
           (set-to-function (collect-image x set-of-tuples)))
       (get-char-function set-of-tuples)))
