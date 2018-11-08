; (defpackage :models 
;   (:use :common-lisp))
; 
; (in-package models)

(load "util/setutils.lisp")

(defparameter *generate-entity* (let ((count 0 ))
                                  #'(lambda () (incf count))) "a closure for the stock of domain entities")

(defparameter *entity-domain* (mapcar 
                                 #'(lambda (x) (or x (funcall *generate-entity*)))
                                 (make-list 4 :initial-element nil)))

(defparameter *content-vocabulary* '((0 john mary bill sue)
									 (1 happy green book dog cat human sleeps walks)
									 (2 loves hates chases)
									 ))

(defparameter *function-vocabulary* '((is #'identity)
									  (not #'(lambda (f)
											  #'(lambda (x) (not (funcall f x)))))))

(defun read-vocab-from-file (vocab-path-string)
  (with-open-file (instr (make-pathname :name vocab-path-string) :direction :input)
    (setf *content-vocabulary* (read instr nil 'eof))))

(defun process-vocab-item (vocab-item)
  (let ((arity (car vocab-item))
        (names (cdr vocab-item)))
    (if (plusp arity)
      (mapcar 
        #'(lambda (x) (cons x (generate-tuples *entity-domain* arity)))
        names)
      (mapcar 
        #'(lambda (x) (cons x (random-pick *entity-domain*)))
        names))))

(defun generate-model ()
    (do ((vocab-list *content-vocabulary*  (cdr vocab-list))
         (model nil (append (process-vocab-item (car vocab-list)) model)))
      ((endp vocab-list) model)))

(defparameter *model* (generate-model))

(defun display-model ()
  (let* ((vocab (sort *content-vocabulary* #'< :key #'car))
         (arities (mapcar #'car vocab)))
    (format t "~%The domain of entities: ~A~%" *entity-domain*)
    (dolist (arity arities)
      (format t "~%~A-place names: ~%" arity)
      (dolist (name (cdr (assoc arity *content-vocabulary*)))
        (format t "~A:    ~A~%" name (let ((value (cdr (assoc name *model*))))
									   (if (and (consp value ) (consp (car value)))
										 (mapcar #'reverse value)
										 value)))))))

(defun construct-interpretation ()
  (let ((if-table (make-hash-table)))     ; interpretation function table
	(dolist (item *model*)
	  (let ((vocab-item (car item))       ; vocabulary item
			(interp (cdr item)))         ; set-theoretic interpretation 
		(if (listp interp)
		  (setf (gethash vocab-item if-table) (set-to-function interp))
		  (setf (gethash vocab-item if-table) interp))))
	(dolist (item *function-vocabulary* if-table)
	  (let ((vocab-item (car item))    
			(interp (cadr item)))      
		(setf (gethash vocab-item if-table) (eval interp))))))

(defparameter *interpretation* (construct-interpretation))

(defun interpret (form)
  (if (consp form)
	(funcall (interpret (car form)) (interpret (cadr form)))
	(gethash form *interpretation*)))

(defun refresh-model ()
  (setf *model* (generate-model))
  (setf *interpretation* (construct-interpretation)))
