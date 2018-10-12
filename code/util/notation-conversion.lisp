(defun convert-notation (form)
  (if (atom form)
    form
    (list (convert-notation (cadr form))
          (convert-notation (car form))
          (convert-notation (caddr form)))))
