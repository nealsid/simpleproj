(ert-deftest simple-precondition-failure ()
  "Verifies that a simple bad precondition fails"
  (should-error
   (funcall (lambda ()
              (new-precondition
               ((> 0 1) "0 is NOT > 1!")
               ((< 1 2) "foo"))))
   :type 'error))

(ert-deftest precondition-failure-100-preconditions ()
  "Precondition test with 100 preconditions"
  (let* ((precondition-clauses (cl-loop for i from 0 to 100
                                       when (< i 99)
                                       collect `((> ,i ,(1- i)) '("%s is not greater than %s" ,i ,(1- i)))
                                       when (= i 99)
                                       collect `((> ,i ,(1+ i)) '("%s is not greater than %s" ,i ,(1+ i)))))
         (error-result (should-error
                        (funcall (lambda ()
                                   (eval `(new-precondition ,@precondition-clauses))))
                        :type 'error)))
    (should (string-equal
             (cadr error-result)
             "99 is not greater than 100"))))
