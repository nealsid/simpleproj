;; Some Emacs helper functions to interact with projects.

(require 'flymake)

(define-error 'compilation-commands-missing "No compilation commands json file in build root")
(define-minor-mode simpleproj-minor-mode "Simple Project Minor Mode" :lighter " Sproj")

(setq simpleproj-projects '())

(cl-defstruct simple-project
  "SimpleProj Project Structure"
  (project-name nil :documentation "Project name")
  (project-short-name nil :documentation "Abbreviated project name")
  (source-root nil :documentation "Source root for the project")
  (build-root nil :documentation "Build root for the project")
  (compile-commands-command nil :documentation "Command to generate compile_commands.json file")
  (filename-to-compile-command-trie nil :documentation "Trie of filename to compilation command"))


(defun simpleproj-find-matching-project-for-buffer ()
  "Returns the matching project for the current buffer or NIL if none found."
  (let* ((filename (buffer-file-name))
         (matching-projects (seq-filter (lambda (project)
                                          (if (string-prefix-p (simple-project-source-root project) filename t)
                                              project
                                            nil))
                                        simpleproj-projects)))
    (cond ((= (length matching-projects) 1)
           (nth 0 matching-projects))
          ((> (length matching-projects) 1)
           (error "More than 1 project matching %s found." filename))
          nil)))

(defun simpleproj-find-file-hook ()
  "Hook to determine if the file being opened is contained within a SimpleProj project entry, and, if, so, turn on simpleproj-minor-mode."
  (let* ((filename (buffer-file-name))
         (matching-projects (simpleproj-find-matching-project-for-buffer)))
    (cond (matching-projects
           (simpleproj-minor-mode)))))

(add-hook 'find-file-hook 'simpleproj-find-file-hook)

;; Specify a depth of 10 so that simpleproj-configure-flymake happens
;; after simpleproj-build-compilation-trie-hook.
(add-hook 'simpleproj-minor-mode-hook 'simpleproj-build-compilation-trie-hook 10)
(add-hook 'simpleproj-minor-mode-hook 'simpleproj-configure-flymake 10)

(defun simpleproj-flymake-cc-advice-change-wd (orig-function report-fn &rest args)
  "Advice for flymake-cc to change the working directory to what is
specified in compile_commands.json.  If the current buffer is not
part of a simple project, just call the original function with no
change in environment."

  (let* ((sproj (simpleproj-find-matching-project-for-buffer))
         (default-directory (cond (sproj (simpleproj-get-compilation-command-wd-for-buffer sproj))
                                  (t default-directory))))
    (funcall orig-function report-fn args)))

(defun simpleproj-configure-flymake ()
  (let ((sproj (simpleproj-find-matching-project-for-buffer)))
    (setq flymake-cc-command
          (split-string (simpleproj-get-compilation-command-for-buffer sproj)))
    (flymake-mode)
    ;; It would be better to specify that the advice is buffer-local
    ;; so that we only advise flymake-cc in buffers that are part of a
    ;; simple project, but buffer-local advice only works for
    ;; functions that are called by storing the function symbol in a
    ;; variable and advising the variable (AFAICT), so we just have to
    ;; advise it globally and do nothing in the case when the buffer
    ;; is not related to a simple project.
    (advice-add 'flymake-cc :around #'simpleproj-flymake-cc-advice-change-wd)))

(defun simpleproj-build-compilation-trie-hook ()
  (and simpleproj-minor-mode
       (let ((sproj (simpleproj-find-matching-project-for-buffer))
             (gc-cons-threshold 10000000000)) ; Abitrarily large
                                              ; number to pause GC
                                              ; while JSON parsing and
                                              ; trie building are
                                              ; happening.
         (when (simpleproj-compilation-command-json-exists-p sproj)
           (setf (simple-project-filename-to-compile-command-trie sproj)
                 (simpleproj-build-compilation-command-trie
                  (make-compile-commands-json (concat (simple-project-build-root sproj) "/compile_commands.json"))))))))

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

(add-simple-project :project-name "Linux kernel"
                    :project-short-name "Kernel"
                    :source-root "/home/nealsid/git/linux"
                    :build-root "/home/nealsid/git/linux"
                    :compile-commands-command "make compile_commands.json")

(defun simpleproj-compilation-command-json-exists-p (sproj-project)
  (file-exists-p (concat (simple-project-build-root sproj-project) "/compile_commands.json")))

(defun remove-unnecessary-command-line-options-for-flycheck (command-line)
  (replace-regexp-in-string " \\(\\(-f[^ ]+\\)\\|\\(-g[^ ]*\\)\\|\\(-pg\\)\\|\\(-m[^ ]+\\)\\)" "" command-line))

(defun simpleproj-build-compilation-command-trie (json)
  (let ((filename-trie nil))
    (mapc (lambda (x)
            (let ((file-full-path (gethash "file" x))
                  (file-compilation-command (remove-unnecessary-command-line-options-for-flycheck (gethash "command" x)))
                  (file-compilation-wd (gethash "directory" x)))
              (setq filename-trie (add-string-to-trie filename-trie
                                                      file-full-path
                                                      (list file-compilation-wd
                                                            file-compilation-command)
                                                      0))))
          json)
    filename-trie))

(defun simpleproj-get-compilation-command-wd-for-buffer (sproj-project)
  (nth 0 (lookup-string (simple-project-filename-to-compile-command-trie sproj-project)
                        (buffer-file-name))))

(defun simpleproj-get-compilation-command-for-buffer (sproj-project)
  (nth 1 (lookup-string (simple-project-filename-to-compile-command-trie sproj-project)
                        (buffer-file-name))))

(defun compile-using-project-compilation-command ()
  (interactive)
  (let* ((project (simpleproj-find-matching-project-for-buffer))
         (compiler-command (simpleproj-get-compilation-command-for-buffer project))
         (full-compile-command (concat "cd " (simple-project-build-root project) " && " compiler-command)))
    (compile full-compile-command)))

(defun file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun make-compile-commands-json (pathname)
  (json-parse-string (file-to-string pathname)))
