;;; simpleproject --- A project to make some project related tasks easier (for me)
;;;
;;; Commentary:
;;;
;;; SimpleProj tries to tie together a compilation command
;;; database with flymake, as well as some tags- and file- related
;;; management tasks.

;;; Code:

(require 'flymake)

(define-minor-mode simpleproj-minor-mode "Simple Project Minor Mode." :lighter " Sproj")
(add-hook 'find-file-hook 'simpleproj-turn-on-simpleproj-if-project-contains-visited-file)

(cl-defstruct simple-project
  "SimpleProj Project Structure."
  (project-name nil :documentation "Project name")
  (project-short-name nil :documentation "Abbreviated project name")
  (source-root nil :documentation "Source root for the project")
  (build-root nil :documentation "Build root for the project")
  (compile-commands-command nil :documentation "Command to generate compile_commands.json file")
  (-db nil :documentation "(not meant for use) Variable containing reference to db"))

(defun simpleproj-turn-on-simpleproj-if-project-contains-visited-file ()
  "Hook to determine if the file being opened is contained within a
SimpleProj project entry, and, if so, turn on `simpleproj-minor-mode'."
  (let* ((filename (buffer-file-name))
         (matching-projects (simpleproj-find-matching-project-for-buffer)))
    (cond (matching-projects
           (simpleproj-minor-mode)))))

(add-hook 'simpleproj-minor-mode-hook 'simpleproj-open-db-for-project 10)
(add-hook 'simpleproj--db-ready-hook 'simpleproj-configure-flymake)

(defvar flymake-cc-command) ;; to avoid warnings

(defun simpleproj-configure-flymake ()
  "Function meant to be called as a hook when `simpleproj-minor-mode'
is enabled.  Turns on flymake and advises `flymake-cc' in order to
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

(defun simpleproj-compilation-command-json-exists-p (sproj-project)
  (file-exists-p (concat (simple-project-build-root sproj-project) "/compile_commands.json")))

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

(setq simpleproj-projects '())
(define-error 'compilation-commands-missing "No compilation commands json file in build root")
(add-simple-project :project-name "Linux kernel"
                    :project-short-name "Kernel"
                    :source-root "/home/nealsid/git/linux"
                    :build-root "/home/nealsid/git/linux"
                    :compile-commands-command "make compile_commands.json")

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

(defun simpleproj-flymake-cc-advice-change-wd (orig-function report-fn &rest args)
  "Function meant to be used as advice for `flymake-cc'.  Change the
working directory to what is specified in compile_commands.json
before invoking `flymake-cc'.  If the current buffer is not part
of a simple project, just call (ORIG-FUNCTION REPORT-FN ARGS)
with no change in environment."
  (let* ((sproj (simpleproj-find-matching-project-for-buffer))
         (default-directory (cond (sproj (simpleproj-get-compilation-command-wd-for-buffer sproj))
                                  (t default-directory))))
    (funcall orig-function report-fn args)))
