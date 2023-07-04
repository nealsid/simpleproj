;; Simple task list
;;
;; This is just a list of functions, where a failure in executing one
;; function aborts executing the remainder.  You can also restart
;; execution at a single function, in which case all remaining
;; functions in the list will also be executed.  It's something like a
;; dependency graph, except it's just a list and there's no
;; parallelism (for now), for simplicity.

(cl-defstruct sp-task-list
  "Simple task list"
  (:tasks '() :documentation "List of functions to execute"))

(defun sp-execute-task-list (task-list &optional start-num)
  (preconditions
   `((sp-task-list-p task-list) ("Task list wrong type: %s" (type-of task-list))))

  (cl-loop for task-num from 0 to (length task-list)
           for res = (funcall (nth task-num task-list))
           when (not (equal t res)) return task-num))


(sp-execute-task-list `(,(lambda () t),(lambda () (+ 3 5))))
