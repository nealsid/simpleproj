(load-file "../util.el")

(ert-deftest simple-precondition-failure ()
  "Verifies that a simple bad precondition fails"
  (should-error
   (funcall (lambda ()
              (new-precondition
               ((> 0 1) "0 is NOT > 1!")
               ((< 1 2) "foo"))))
   :type 'error))

(ert-deftest precondition-with-format-string ()
  "Verifies a simple precondition with a format string"
  (let* ((variable-value "x")
         (error-result (should-error
                        (funcall (lambda ()
                                   (new-precondition
                                    ((> 0 1) ("bad parameter %s" variable-value)))))
                        :type 'error)))
    (should (string-equal
             (cadr error-result)
             "bad parameter x"))))

(defun generate-n-precondition-clauses (n failure-clause-num)
  "Generate n precondition clauses, with the `failure-clause-num`th
clause (0-based index) failing, or -1 for none to fail"
  (cl-loop for i from 0 to n
           when (not (= i failure-clause-num))
           collect `((> ,i ,(1- i)) ("%s is not greater than %s" ,i ,(1- i)))
           when (= i failure-clause-num)
           collect `((> ,i ,(1+ i)) ("%s is not greater than %s" ,i ,(1+ i)))))

(cl-loop for failure-clause in '(0 49 99)
         do (eval `(ert-deftest ,(intern (format "precondition-failure-100-preconditions-%s-clause-fail" failure-clause)) ()
                     "Precondition test with 100 preconditions, 1 failing"
              (let* ((precondition-clauses (generate-n-precondition-clauses 100 ,failure-clause))
                     (error-result (should-error
                                    (funcall (lambda ()
                                               (eval `(new-precondition ,@precondition-clauses))))
                                    :type 'error)))
                (should (string-equal
                         (cadr error-result)
                         (format "%s is not greater than %s" ,failure-clause (1+ ,failure-clause))))))))
