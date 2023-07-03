;; Some Emacs helper functions to interact with projects.

(require 'flymake)

(define-minor-mode simpleproj-minor-mode "Simple Project Minor Mode" :lighter " Sproj")
(add-hook 'find-file-hook 'simpleproj-find-file-hook)
;; Specify a depth of 10 so that simpleproj-configure-flymake happens
;; after simpleproj-build-compilation-trie-hook.
(add-hook 'simpleproj-minor-mode-hook 'simpleproj-build-compilation-trie-hook 10)
(add-hook 'simpleproj-minor-mode-hook 'simpleproj-configure-flymake 10)

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
  "Hook to determine if the file being opened is contained within a
SimpleProj project entry, and, if, so, turn on simpleproj-minor-mode."
  (let* ((filename (buffer-file-name))
         (matching-projects (simpleproj-find-matching-project-for-buffer)))
    (cond (matching-projects
           (simpleproj-minor-mode)))))

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
  "Function meant to be called as a hook when simpleproj-minor-mode
is enabled.  Turns on flymake and advises flymake-cc in order to
change the working directory while the compiler is being invoked."
  (let ((sproj (simpleproj-find-matching-project-for-buffer)))
    ;; TODO: this needs to be buffer-specific, as flymake-cc is not
    ;; read in a buffer-local fashion.
    (setq flymake-cc-command
          'simpleproj-get-compilation-command-for-flymake)
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
         (language-and-stdin-option (string-join (list " -x" gcc-language-option "-") " "))
         (remove-filename-regexp (concat " [^ ]*" compilation-file-filename "\\( \\|$\\)")))
    (replace-multiple-regexps `(("$"                     . ,language-and-stdin-option)
                                (" -o +[^ ]+\\( \\|$\\)" . " ")
                                (,remove-filename-regexp . " "))
                              command-line)))

(defun remove-unnecessary-command-line-options-for-flymake (command-line)
  (replace-regexp-in-string " \\(\\(-f[^ ]+\\)\\|\\(-g[^ ]*\\)\\|\\(-pg\\)\\|\\(-m[^ ]+\\)\\)" "" command-line))

(defun simpleproj-build-compilation-command-trie (json)
  (let ((filename-trie nil))
    (mapc (lambda (x)
            (let* ((file-full-path (gethash "file" x))
                   (file-compilation-command
                    (transform-build-command-line-into-flymake-command-line
                     (remove-unnecessary-command-line-options-for-flymake (gethash "command" x)) file-full-path))
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

(defun simpleproj-get-compilation-command-for-flymake ()
  (split-string (simpleproj-get-compilation-command-for-buffer (simpleproj-find-matching-project-for-buffer)) " "))

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

(setq simpleproj-projects '())
(define-error 'compilation-commands-missing "No compilation commands json file in build root")
