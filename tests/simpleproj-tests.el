(require 'gv)
(require 'cl)

(defun create-test-compile-command-json (project-dir)
  "Creates a compile_commands.json file in the directory specified"
   (let* ((json-array (vector (make-hash-table)))
          (cmd-hash (aref json-array 0)))
     (puthashm "command" "gcc main.c"
               "directory" project-dir
               "file" (concat project-dir "main.c") cmd-hash)
     (with-current-buffer-close
         (find-file-noselect (concat project-dir "compile_commands.json") t t nil)
       (erase-buffer)
       (insert (json-serialize json-array))
       (save-buffer)))
   (delete-file (concat project-dir "sproj-compilation-commands.sqlite3")))

(defmacro with-project-and-directory (project-dir-name-binding &rest forms)
  "Macro to wrap a test case body that creates bindings for the
unique project directory for the test case as well as a
SimpleProj project that refers to the directory."
  `(let* ((,project-dir-name-binding (concat (make-temp-file "simpleproj-" t) "/"))
          (simpleproj-projects (list (make-simple-project
                                      :project-name "Hello, World"
                                      :project-short-name "helloworld"
                                      :source-root ,project-dir-name-binding
                                      :build-root ,project-dir-name-binding))))
     (progn
       (create-test-compile-command-json ,project-dir-name-binding)
       ,@forms)))

(defmacro with-current-buffer-close (buffer-or-name &rest body)
  `(progn
     (with-current-buffer ,buffer-or-name ,@body)
     (kill-buffer ,buffer-or-name)))

(ert-deftest open-one-file-project ()
  "Opens a project with one file"
  (with-project-and-directory
   project-dir
   (with-current-buffer-close
       (find-file-noselect (concat project-dir "main.c"))
     (should (eq simpleproj-minor-mode t))
     ;; intentional to use EQ since the buffer local variable should
     ;; be a reference to an item in the global list.
     (should (eq simpleproj-project (nth 0 simpleproj-projects)))
     (should (equal simpleproj-flymake-command-line '("gcc" "-x" "c" "-fsyntax-only" "-")))
     (should (string-equal simpleproj-flymake-working-directory project-dir))
     (should (eq flymake-mode t)))))

(ert-deftest open-one-file-no-command-line-in-db ()
  "Opens a project with one file that does not have a command line in the database"
  (with-project-and-directory
   project-dir
   (with-current-buffer-close
       (find-file-noselect (concat project-dir "kldjflkjd.c"))
     (should (eq simpleproj-minor-mode t))
     (should (eq simpleproj-project (nth 0 simpleproj-projects)))
     (should (not (bound-and-true-p simpleproj-flymake-command-line)))
     (should (not (bound-and-true-p simpleproj-flymake-working-directory)))
     (should (eq flymake-mode nil)))))

(ert-deftest open-one-file-multiple-containing-projects ()
  "Opens a file that is contained in multiple projects in order to ensure an error is raised."
  (with-project-and-directory
   project-dir
   (push (make-simple-project
          :project-name "Hello, World #2"
          :project-short-name "helloworldno2"
          :source-root project-dir
          :build-root project-dir) simpleproj-projects)
   (with-current-buffer-close
       (find-file-noselect (concat project-dir "kldjflkjd.c"))
     (should (eq simpleproj-minor-mode nil))
     (should (not (bound-and-true-p simpleproj-flymake-command-line)))
     (should (not (bound-and-true-p simpleproj-flymake-working-directory)))
     (should (not (bound-and-true-p simpleproj-project))))))
