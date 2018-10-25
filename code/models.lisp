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

(defparameter *vocabulary* '((0 john mary bill sue)
                             (2 loves hates chases)
                             (1 happy green book dog cat human sleeps walks)
                             ))

(defun read-vocab-from-file (vocab-path-string)
  (with-open-file (instr (make-pathname :name vocab-path-string) :direction :input)
    (setf *vocabulary* (read instr nil 'eof))))

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
    (do ((vocab-list *vocabulary*  (cdr vocab-list))
         (model nil (append (process-vocab-item (car vocab-list)) model)))
      ((endp vocab-list) model)))

(defun display-model (model)
  (let* ((vocab (sort *vocabulary* #'< :key #'car))
         (arities (mapcar #'car vocab)))
    (format t "~%The domain of entities: ~A~%~%" *entity-domain*)
    (dolist (arity arities)
      (format t "~%~A-place names: ~%~%" arity)
      (dolist (name (cdr (assoc arity *vocabulary*)))
        (format t "~A:    ~A~%" name (cdr (assoc name model)))))))
