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
                      (cond ((raise-via-error error-clause )
                             `(error ,error-clause))
                            ((listp error-clause)
                             `(error ,@error-clause)))))))

(defun puthashm (&rest args)
       "Wrapper around puthash to support storing multiple key/values in
one call by the user. Arguments except last are key & value,
repeated, and last argument is the hash table."
       (cl-assert (cl-oddp (length args)))
       (cl-assert (length> args 2))
       (let ((ht (car (last args))))
         (cl-assert (hash-table-p ht))
         (cl-loop for (key value) on args by 'cddr
                  for i from 0 to (length args) by 2
                  do (puthash key value ht)
                  when (eq i (- (length args) 3)) return t)))
