(ert-deftest simple-precondition-failure ()
  "Verifies that a simple bad precondition fails"
  (setq my-func (lambda ()
                  (new-precondition
                   ((> 0 1) "0 is NOT > 1!")
                   ((< 1 2) "foo")))))
