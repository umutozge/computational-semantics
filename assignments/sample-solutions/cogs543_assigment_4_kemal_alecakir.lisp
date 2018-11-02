;;;;Assigment 4
;;;@author:alecakir

;;Note: In case the solution is correct, it could be shared. In other case, this could be misleading.



(defun master-transformation (app-str)
	(if (listp (car app-str))
			(transformation app-str)
			(funcall (car app-str) (cadr app-str))
		 )
	)

(defun transformation (app-str)
	(cond ((endp app-str) nil)
		((listp (car app-str))  (funcall (transformation (car app-str)) (cadr app-str)))
		(t #'(lambda (x) (funcall  (car app-str) (cadr app-str) x)))
		)
	)

;;;Test  code

(defparameter *test-cases* '((((CONCATSTRING "li") "st") 4)
							((+ 2) 3)
							(null nil))
)


(defun main ()
	(dolist (x *test-cases* 'done) 
		(format t "~D is transformed to: ~D~%" (write-to-string x) (master-transformation x) )
		)
	;(transformation '(((CONCATSTRING "li") "st") 4))
	)

;;helper method to create test case
;;takes two string parameter and then concatenates strings to create function name
(defun concatString (ar1 ar2)
	(setq result  (read-from-string (concatenate 'string ar1 ar2))))

