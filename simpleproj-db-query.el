(require 'sqlite)

(defvar simpleproj--db-ready-hook nil
  "Hook run after the database for the current project is initialized")

(defun simpleproj-query-for-file-flymake-settings (sproj file-name)
  (let ((db (simple-project--db sproj)))
    (cl-assert db)
    (let ((query-result (sqlite-select db "select working_directory, compile_command from compilation_commands where file_name = ?" (list file-name))))
      (cl-assert (length= query-result 1))
      (cl-assert (length= (nth 0 query-result) 2))
      (nth 0 query-result))))
