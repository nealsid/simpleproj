(load-file "../simpleproj.el")
(load-file "../simpleproj-flymake.el")
(load-file "../simpleproj-db-init-and-load.el")
(load-file "../simpleproj-db-query.el")
(load-file "../simpleproj-cc-json-functions.el")
(require 'gv)
(require 'cl)

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

(ert-deftest open-one-file-project ()
  "Opens a project with one file"
  ;; generate json file
  (let* ((json-array (vector (make-hash-table)))
         (cmd-hash (aref json-array 0)))
    (puthashm "command" "gcc main.c"
              "directory" default-directory
              "file" (expand-file-name (concat default-directory "main.c")) cmd-hash)
    (with-current-buffer
        (find-file-noselect "compile_commands.json")
      (erase-buffer)
      (insert (json-serialize json-array))
      (save-buffer)))
  (delete-file (concat default-directory "sproj-compilation-commands.sqlite3"))
  (let ((simpleproj-projects (list (make-simple-project
                              :project-name "Hello, World"
                              :project-short-name "helloworld"
                              :source-root (expand-file-name default-directory)
                              :build-root (expand-file-name default-directory)))))
    (with-current-buffer
        (find-file-noselect "main.c")
      (should (eq simpleproj-minor-mode t))
      (should (eq simpleproj-project (nth 0 simpleproj-projects)))
      (should (eq flymake-mode t)))))


(ert-deftest open-one-file-no-command-line-in-db ()
  "Opens a project with one file that does not have a command line in the database"
  ;; TODO dedup code with above test.
  ;; generate json file
  (let* ((json-array (vector (make-hash-table)))
         (cmd-hash (aref json-array 0)))
    (puthashm "command" "gcc main.c"
              "directory" default-directory
              "file" (expand-file-name (concat default-directory "main.c")) cmd-hash)
    (with-current-buffer
        (find-file-noselect "compile_commands.json")
      (erase-buffer)
      (insert (json-serialize json-array))
      (save-buffer)))
  (delete-file (concat default-directory "sproj-compilation-commands.sqlite3"))
  (let ((simpleproj-projects (list (make-simple-project
                              :project-name "Hello, World"
                              :project-short-name "helloworld"
                              :source-root (expand-file-name default-directory)
                              :build-root (expand-file-name default-directory)))))
    (with-current-buffer
        (find-file-noselect "kldjflkjd.c")
      (should (eq simpleproj-minor-mode t))
      (should (eq simpleproj-project (nth 0 simpleproj-projects)))
      (should (eq flymake-mode nil)))))
