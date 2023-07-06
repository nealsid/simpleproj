(require 'sqlite)

(defvar simpleproj--db-ready-hook nil
  "Hook run after the database for the current project is initialized")

(defun simpleproj-open-db-for-project (&optional sproj)
  (let ((sproj (or sproj (simpleproj-find-matching-project-for-buffer))))
    (cond ((simpleproj-database-should-be-created sproj)
           (simpleproj-create-database-and-initialize-sqlite sproj)
           (parse-json-into-sqlite-table sproj))
          (t (simpleproj-initialize-sqlite-for-project sproj)))
    (run-hooks 'simpleproj--db-ready-hook)))
