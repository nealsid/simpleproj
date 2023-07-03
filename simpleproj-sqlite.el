(require 'sqlite)

(defun create-database-for-simple-project (sproj)
  (let ((sproj-sqlite-db-filename (concat (simple-project-build-root sproj) "/sproj-compilation-commands.sqlite3")))
    ;; (when (file-exists-p sproj-sqlite-db-filename)
    ;;   (error "Sqlite database for project \"%s\" already exists" (simple-project-project-name sproj)))

    (let* ((db (sqlite-open sproj-sqlite-db-filename))
           (compile-commands-json-path (concat (simple-project-build-root sproj) "/compile_commands.json"))
           (table-create-dml "CREATE TABLE compilation_commands (file_name text not null primary key, compile_command text, working_directory text)"))
      (sqlite-execute db table-create-dml)
      (sqlite-close db))))

(defun parse-json-into-sqlite-table (sproj)
  (let* ((gc-cons-threshold 10000000000)
         (sproj-sqlite-db-filename (concat (simple-project-build-root sproj) "/sproj-compilation-commands.sqlite3"))
         (json-load-sql (format "insert into compilation_commands (file_name, compile_command, working_directory) \
                                                        values (?, ?, ?)"))
         (db (sqlite-open sproj-sqlite-db-filename))
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
              (condition-case raised-error
                  (sqlite-execute db json-load-sql (list file-full-path file-compilation-command file-compilation-wd))
                (sqlite-error
                 (let ((sqlite-error-message (cadr raised-error)))
                   (cond ((string-equal "UNIQUE constraint failed: compilation_commands.file_name"
                                        sqlite-error-message) nil)
                         (t (progn
                              (sqlite-commit db)
                              (sqlite-close db)
                              (signal (car raised-error) (cdr raised-error))))))))))
          json)
    (sqlite-commit db)
    (sqlite-close db)))
