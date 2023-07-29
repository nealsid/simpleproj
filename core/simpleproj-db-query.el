(require 'sqlite)

(defun simpleproj-query-for-file-flymake-settings (sproj file-name)
  (let ((db (simple-project--db sproj)))
    (cl-assert db)
    (let ((query-result (sqlite-select db "select working_directory, compile_command from compilation_commands where file_name = ?" (list file-name))))
      (cl-assert (length< query-result 2))
      (cond ((length= query-result 1)
             (cl-assert (length= (nth 0 query-result) 2))
             (nth 0 query-result))
            nil))))

(defun simpleproj-update-flymake-command-line-for-file (sproj file-name compile-command)
  (local ((db (simple-project--db sproj))
          (update-statement "update compilation_commands set compile_command = ? where file_name = ?" ))
    (cl-assert (eq 1 (sqlite-execute db update-statement (list compile_command file_name))))))
