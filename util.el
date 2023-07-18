(defun raise-via-error (clause)
  "Returns t if the clause should be raised via error, nil otherwise"
  (or (stringp clause)
      (and (symbolp clause)
           (get clause 'error-conditions))))

(defmacro new-precondition (&rest forms)
   `(cond ,@(cl-loop
             for (precondition-form error-clause)
             in forms
             collect (list
                      `(not ,precondition-form)
                      (cond ((raise-via-error error-clause)
                             `(error ,error-clause))
                            ((listp error-clause)
                             `(error ,@error-clause)))))))
