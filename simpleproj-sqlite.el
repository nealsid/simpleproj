(require 'sqlite3)

(defun create-database-for-simple-project (sproj)
  (let ((sproj-sqlite-db-filename (concat (simple-project-build-root sproj) "/sproj-compilation-commands.sqlite3")))
    ;; (when (file-exists-p sproj-sqlite-db-filename)
    ;;   (error "Sqlite database for project \"%s\" already exists" (simple-project-project-name sproj)))

    (let* ((db (sqlite-open sproj-sqlite-db-filename))
           (compile-commands-json-path (concat (simple-project-build-root sproj) "/compile_commands-1-entry.json"))
           (table-create-dml "CREATE TABLE compilation_commands (file_name text not null primary key, compile_command textworking_directory text)"))
      (sqlite-execute db table-create-dml)
      (sqlite-close db))))

(defun parse-json-into-sqlite-table (sproj)
  ;; Sqlite does not expose READFILE through the API, only through the
  ;; command shell, and loading the JSON is much simpler this way, so
  ;; for this particular step, we shell out to the command line
  ;; shell.
  (let ((sproj-sqlite-db-filename (concat (simple-project-build-root sproj) "/sproj-compilation-commands.sqlite3"))
        (json-load-sql (format "insert into compilation_commands (file_name, compile_command, working_directory)\
                                select json_extract(value, '$.file'),
                                       json_extract (value, '$.command'),\
                                       json_extract(value, '$.directory') \
                                  from json_each(readfile('%s'))"
                               (concat (simple-project-build-root sproj) "/compile_commands.json"))))
    (start-process "sqlite" (get-buffer "sqlite-process") "sqlite3" sproj-sqlite-db-filename
                   "-cmd" json-load-sql)))
