(ert-deftest open-one-file-project ()
  "Opens a project with one file"
  (add-simple-project :project-name "Hello, World"
                      :project-short-name "helloworld"
                      :source-root default-directory
                      :build-root default-directory)
  ;; generate json file
  (let* ((json-array (vector (make-hash-table)))
         (cmd-hash (aref json-array 0)))
    (puthash "command" "gcc main.c" cmd-hash)
    (puthash "directory" default-directory cmd-hash)
    (puthash "file" (concat default-directory "main.c") cmd-hash)
    (with-current-buffer
        (find-file-noselect "compile_commands.json")
      (erase-buffer)
      (insert (json-serialize json-array))
      (save-buffer))))
