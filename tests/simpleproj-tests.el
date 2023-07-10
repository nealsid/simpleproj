(load-file "../simpleproj.el")
(load-file "../simpleproj-db-init-and-load.el")
(load-file "../simpleproj-db-query.el")
(load-file "../simpleproj-cc-json-functions.el")
(require 'gv)

(ert-deftest open-one-file-project ()
  "Opens a project with one file"
  ;; generate json file
  (let* ((json-array (vector (make-hash-table)))
         (cmd-hash (aref json-array 0)))
    (puthash "command" "gcc main.c" cmd-hash)
    (puthash "directory" default-directory cmd-hash)
    (puthash "file" (expand-file-name (concat default-directory "main.c")) cmd-hash)
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
      (should (eq simpleproj-minor-mode t)))))
