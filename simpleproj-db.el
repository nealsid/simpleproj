(require 'sqlite)

(defun simpleproj-open-db-for-project (sproj)
  (cond ((simpleproj-database-exists-and-recent sproj)
         (progn
           (simpleproj-initialize-sqlite-for-project sproj)
           t))
        (t (progn
             (simpleproj-create-database-and-initialize-sqlite sproj)
             (parse-json-into-sqlite-table sproj)))))

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
    (mapc (lambda (x)
            (let* ((file-full-path (gethash "file" x))
                   (file-compilation-command
                    (transform-build-command-line-into-flymake-command-line
                     (remove-unnecessary-command-line-options-for-flymake (gethash "command" x)) file-full-path))
                   (file-compilation-wd (gethash "directory" x)))

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
          json)
    (sqlite-commit db)))

(defun simpleproj-database-exists-and-recent (sproj)
  (let ((sproj-sqlite-db-filename (concat (simple-project-build-root sproj) "/sproj-compilation-commands.sqlite3"))
        (compile-commands-json (concat (simple-project-build-root sproj) "/compile_commands.json")))
    (cond ((not (file-exists-p sproj-sqlite-db-filename))
           nil)
          ((file-newer-than-file-p sproj-sqlite-db-filename compile-commands-json)
           t))))
