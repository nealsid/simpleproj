(load-file "../util.el")
(load-file "../simpleproj.el")
(load-file "../simpleproj-flymake.el")
(load-file "../simpleproj-db-init-and-load.el")
(load-file "../simpleproj-db-query.el")
(load-file "../simpleproj-cc-json-functions.el")
(require 'gv)
(require 'cl)

(defmacro with-temp-project-directory (dir-name-binding &rest forms)
  `(let* ((,dir-name-binding (concat (make-temp-file "simpleproj-" t) "/")))
    (progn ,@forms)))

(defmacro with-current-buffer-close (buffer-or-name &rest BODY)
  `(with-current-buffer buffer-or-name body)
  `(kill-buffer buffer-or-name))

(ert-deftest open-one-file-project ()
  "Opens a project with one file"
  (with-temp-project-directory
   project-dir
  ;; generate json file
   (let* ((json-array (vector (make-hash-table)))
          (cmd-hash (aref json-array 0)))
     (puthashm "command" "gcc main.c"
               "directory" project-dir
               "file" (concat project-dir "main.c") cmd-hash)
     (with-current-buffer
         (find-file-noselect (concat project-dir "compile_commands.json"))
       (erase-buffer)
       (insert (json-serialize json-array))
       (save-buffer)))
   (delete-file (concat project-dir "sproj-compilation-commands.sqlite3"))
   (let ((simpleproj-projects (list (make-simple-project
                                     :project-name "Hello, World"
                                     :project-short-name "helloworld"
                                     :source-root project-dir
                                     :build-root project-dir))))
     (with-current-buffer
         (find-file-noselect (concat project-dir "main.c"))
       (should (eq simpleproj-minor-mode t))
       (should (eq simpleproj-project (nth 0 simpleproj-projects)))
       (should (eq flymake-mode t))))))

(ert-deftest open-one-file-no-command-line-in-db ()
  "Opens a project with one file that does not have a command line in the database"
  ;; TODO dedup code with above test.
  ;; generate json file
  (with-temp-project-directory
   project-dir
   (message "project dir: %s" project-dir)
   (let* ((json-array (vector (make-hash-table)))
          (cmd-hash (aref json-array 0)))
     (puthashm "command" "gcc main.c"
               "directory" default-directory
               "file" (expand-file-name (concat project-dir "main.c")) cmd-hash)
     (with-current-buffer
         (find-file-noselect (concat project-dir "compile_commands.json"))
       (erase-buffer)
       (insert (json-serialize json-array))
       (save-buffer)))
   (delete-file (concat project-dir "sproj-compilation-commands.sqlite3"))
   (let ((simpleproj-projects (list (make-simple-project
                                     :project-name "Hello, World"
                                     :project-short-name "helloworld"
                                     :source-root project-dir
                                     :build-root project-dir))))
     (with-current-buffer
         (find-file-noselect (concat project-dir "kldjflkjd.c"))
       (should (eq simpleproj-minor-mode t))
       (should (eq simpleproj-project (nth 0 simpleproj-projects)))
       (should (eq flymake-mode nil))))))

(ert-deftest open-one-file-multiple-containing-projects ()
  "Opens a file that is contained in multiple projects in order to ensure an error is raised."
  (with-temp-project-directory
   project-dir
   (message "project dir: %s" project-dir)
   (let ((simpleproj-projects (list (make-simple-project
                                     :project-name "Hello, World"
                                     :project-short-name "helloworld"
                                     :source-root project-dir
                                     :build-root project-dir)
                                    (make-simple-project
                                     :project-name "Hello, World #2"
                                     :project-short-name "helloworldno2"
                                     :source-root project-dir
                                     :build-root project-dir))))
     (with-current-buffer
         (find-file-noselect (concat project-dir "kldjflkjd.c"))
       (should (eq simpleproj-minor-mode nil))
       (should (eq simpleproj-project nil))))))
