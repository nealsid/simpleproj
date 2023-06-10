;; Some Emacs helper functions to interact with compile_commands.json file.


(define-error 'compilation-commands-missing "No compilation commands json file in build root")
(define-minor-mode simpleproj-minor-mode "Simple Project Minor Mode" :lighter " Sproj")

(setq simpleproj-projects '())

(cl-defstruct simple-project
  "SimpleProj Project Structure"
  (project-name nil :documentation "Project name")
  (project-short-name nil :documentation "Abbreviated project name")
  (source-root nil :documentation "Source root for the project")
  (build-root nil :documentation "Build root for the project")
  (compile-commands-command :documentation "Command to generate compile_commands.json file"))

(defun simpleproj-find-file-hook ()
  (let* ((filename (buffer-file-name))
        (matching-projects (seq-filter (lambda (project)
                                         (if (string-prefix-p (project-source project) filename t)
                                             project
                                           nil))
                                       projects)))
    (cond ((> (length matching-projects) 0)
           (simpleproj-minor-mode)))))

(add-hook 'find-file-hook 'simpleproj-find-file-hook)

(add-simple-project :project-name "Linux kernel"
                    :project-short-name "Kernel"
                    :source-root "/home/nealsid/git/linux"
                    :build-root "/home/nealsid/git/linux"
                    :compile-commands-command "make compile_commands.json")

(cl-defun add-simple-project (&key project-name
                                   project-short-name
                                   source-root
                                   build-root
                                   compile-commands-command)
  (add-to-list 'simpleproj-projects
               (make-simple-project :project-name project-name
                                    :project-short-name project-short-name
                                    :source-root source-root
                                    :build-root build-root
                                    :compile-commands-command compile-commands-command)))



(defun project-setting-lookup (setting-name project-settings)
  (cdr (assoc setting-name project-settings)))

;; (defun project-source-root (project-settings)
;;   (cdr (assoc 'project-source project-settings)))

(defun project-build-compilation-hash (json)
  (let ((filename-to-compilation-command (make-hash-table :test 'equal :size 5500)))
    (mapc (lambda (x)
            (let ((file-full-path (gethash "file" x))
                  (file-compilation-command (gethash "command" x)))
              (puthash file-full-path file-compilation-command filename-to-compilation-command)))
          json)
    filename-to-compilation-command))

(defun project-get-compilation-command (fn project-settings)
  (gethash fn (project-compile-command-ht project-settings)))

(defun project-for-current-buffer ()
  (let ((filename (buffer-file-name)))
    (car (seq-filter (lambda (project)
                       (if (string-prefix-p (project-source project) filename t)
                           project
                         nil))
                     projects))))

(defun compile-using-project-compilation-command ()
  (interactive)
  (let* ((project (project-for-current-buffer))
         (compiler-command (project-get-compilation-command (buffer-file-name) project))
         (full-compile-command (concat "cd " (project-build-root project) " && " compiler-command)))
    (compile full-compile-command)))

(defun file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun make-compile-commands-json (pathname)
  (json-parse-string (file-to-string pathname)))
