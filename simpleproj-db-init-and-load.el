;; Functions for initializing and loading a project database.
(defun simpleproj-open-db-for-project (sproj)
  (cond ((simpleproj-database-should-be-created sproj)
         (simpleproj-create-database-and-initialize-sqlite sproj)
         (parse-json-into-sqlite-table sproj))
        (t (simpleproj-initialize-sqlite-for-project sproj)))
  (run-hooks 'simpleproj--db-ready-hook))

(defun simpleproj-initialize-sqlite-for-project (sproj)
  (let ((sproj-sqlite-db-filename (concat (simple-project-build-root sproj) "/sproj-compilation-commands.sqlite3")))
    (cl-assert (file-exists-p sproj-sqlite-db-filename))
    ;; Unfortunately we cannot override flags passed to the C function
    ;; sqlite3_open_v2() in order to specify that the database should
    ;; not be created if it does not exist, which would be an error in
    ;; this code path.
    (setf (simple-project--db sproj) (sqlite-open sproj-sqlite-db-filename))
    t))

(defun simpleproj-create-database-and-initialize-sqlite (sproj)
  (let ((sproj-sqlite-db-filename (concat (simple-project-build-root sproj) "/sproj-compilation-commands.sqlite3")))
    (when (file-exists-p sproj-sqlite-db-filename)
      ;; TODO we probably just want to delete or drop all rows from
      ;; the table instead.
      (delete-file sproj-sqlite-db-filename))
    (let* ((db (sqlite-open sproj-sqlite-db-filename))
           (compile-commands-json-path (concat (simple-project-build-root sproj) "/compile_commands.json"))
           (table-create-dml "CREATE TABLE compilation_commands (file_name text not null primary key, compile_command text, working_directory text, manual_override integer)"))
      (sqlite-execute db table-create-dml)
      (setf (simple-project--db sproj) db))))

(defun parse-json-into-sqlite-table (sproj)
  (let* ((gc-cons-threshold 10000000000)
         (json-load-sql (format "insert into compilation_commands (file_name, compile_command, working_directory) \
                                                        values (?, ?, ?)"))
         (db (simple-project--db sproj))
         (json (make-compile-commands-json (concat (simple-project-build-root sproj) "/compile_commands.json"))))
    (sqlite-transaction db)
    (mapc 'insert-json-entry-into-db json)
    (sqlite-commit db)))

(defun simpleproj-database-should-be-created (sproj)
  (let ((sproj-sqlite-db-filename (concat (simple-project-build-root sproj) "/sproj-compilation-commands.sqlite3"))
        (compile-commands-json (concat (simple-project-build-root sproj) "/compile_commands.json")))
    (cond ((not (file-exists-p sproj-sqlite-db-filename)) t)
          ((file-newer-than-file-p compile-commands-json sproj-sqlite-db-filename) t)
          (t nil))))

(defun insert-json-entry-into-db (json-entry)
  (let* ((file-full-path (gethash "file" json-entry))
         (file-compilation-command
          (transform-build-command-line-into-flymake-command-line
           (gethash "command" json-entry)) file-full-path)
         (file-compilation-wd (gethash "directory" json-entry)))
    ;; Sometimes compile_commands.json files will have
    ;; duplicate entries for the same file, so ignore that
    ;; case by catching the error and continuing.
    ;; TODO use "insert into or ignore" or "insert where not exists"
    (condition-case raised-error
        (sqlite-execute db json-load-sql (list file-full-path file-compilation-command file-compilation-wd))
      (sqlite-error
       (let ((sqlite-error-message (cadr raised-error)))
         (cond ((string-equal "UNIQUE constraint failed: compilation_commands.file_name"
                              sqlite-error-message) nil)
               (t (progn
                    (sqlite-commit db)
                    (signal (car raised-error) (cdr raised-error))))))))))

(defun gcc-language-option-for-extension (extension)
  (cond ((string-equal-ignore-case extension "c") "c")
        ((member extension '("cc" "cpp" "cxx")) "c++")
        (t (error "Invalid extension %s" extension))))

(defmacro replace-multiple-regexps (regexps-and-replacements input-string)
  "Macro which takes a set of regular expressions and replacements and generates a loop over the set to call replace-regexp-in-string.  This makes it easier to read as you can avoid repeatedly setting a temp variable to the result of replace-regexp-in-string in order to pass it to the next call to replace-regexp-in-string. The form of regexps-and-replacements is a list of cons cells, where each cons cell is of the form (regexp . replacement)"
  ;; I'm not sure if this should be a macro, after writing it, there
  ;; does not appear to be a need for expansion at compile time and it
  ;; requires the caller to specify evaluation through backticks and
  ;; commas. TODO rewrite as a function.
  `(let ((temp-string ,input-string))
     (cl-loop for regexp-and-replacement
              in ,regexps-and-replacements
              do (let ((regexp (car regexp-and-replacement))
                       (replacement (cdr regexp-and-replacement)))
                   (setq temp-string (replace-regexp-in-string regexp replacement temp-string))
                   temp-string)
              finally return temp-string)))

(defun transform-build-command-line-into-flymake-command-line (command-line compilation-file-full-path)
  "Flymake expects a compiler command that reads from STDIN, which is not what we read from compile_commands.json. This function will remove options specifying a file on the command line, while also adding something like \"-x <lang> -\".  It is GCC specific and will need some more work to be generalized."
  (let* ((compilation-file-filename (file-name-nondirectory compilation-file-full-path))
         (gcc-language-option (gcc-language-option-for-extension (downcase (file-name-extension compilation-file-full-path))))
         (language-and-stdin-option (string-join (list " -x" gcc-language-option "-fsyntax-only" "-") " "))
         (remove-filename-regexp (concat " [^ ]*" compilation-file-filename "\\( \\|$\\)")))
    (replace-multiple-regexps `(("$"                     . ,language-and-stdin-option)
                                (" -o +[^ ]+\\( \\|$\\)" . " ")
                                (,remove-filename-regexp . " "))
                              command-line)))

(defun simpleproj-compilation-command-json-exists-p (sproj-project)
  (file-exists-p (concat (simple-project-build-root sproj-project) "/compile_commands.json")))
