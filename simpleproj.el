;;; simpleproject --- A project to make some project related tasks easier (for me)
;;;
;;; Commentary:
;;;
;;; SimpleProj tries to tie together a compilation command
;;; database with flymake, as well as some tags- and file- related
;;; management tasks.

;;; Code:

(require 'flymake)

(defvar-local simpleproj-project nil "Buffer-local variable referring to the simple project structure for this buffer")
(defvar-local simpleproj-flymake-command-line nil "Buffer-local variable referring to the Flymake command line for this buffer")
(defvar-local simpleproj-flymake-working-directory nil "Buffer-local variable referring to the Flymake command working directory for this buffer")

(add-hook 'find-file-hook 'simpleproj-turn-on-simpleproj-if-project-contains-visited-file)

(cl-defstruct simple-project
  "SimpleProj Project Structure."
  (project-name nil :documentation "Project name")
  (project-short-name nil :documentation "Abbreviated project name")
  (source-root nil :documentation "Source root for the project")
  (build-root nil :documentation "Build root for the project")
  (compile-commands-command nil :documentation "Command to generate compile_commands.json file")
  (-db nil :documentation "(private) Variable containing reference to db")
  (-visited-buffers nil :documentation "(private) Variable containing list of buffers that are visiting files in this project"))

(defun simpleproj-turn-on-simpleproj-if-project-contains-visited-file ()
  "find-file-hook function to determine if the file being opened is contained within a
SimpleProj project entry, and, if so, turn on `simpleproj-minor-mode'."
  (let* ((matching-project (simpleproj-find-matching-project-for-buffer)))
    (cond (matching-project
           (set-variable 'simpleproj-project matching-project)
           (simpleproj-minor-mode)))))

(define-minor-mode simpleproj-minor-mode "Simple Project Minor Mode." :lighter " Sproj"
  ;; Mode initialization forms that are run before any hooks.
  (simpleproj-open-db-for-project simpleproj-project))

(add-hook 'simpleproj--db-ready-hook 'simpleproj-configure-flymake)

(defvar flymake-cc-command) ;; to avoid warnings

(defun simpleproj-configure-flymake ()
  "Function meant to be called during `simpleproj-minor-mode'
initialization.  Turns on flymake and advises `flymake-cc' in
order to change the working directory while the compiler is being
invoked."
  ;; There should probably be some sanity check in case this
  ;; variable is overwritten by something unrelated to SimpleProj.
  (setq flymake-cc-command
        'simpleproj-get-compilation-command-for-flymake)
  (let ((flymake-settings-for-file
         (simpleproj-query-for-file-flymake-settings
          simpleproj-project
          (buffer-file-name))))
    (setq simpleproj-flymake-working-directory (nth 0 flymake-settings-for-file))
    (setq simpleproj-flymake-command-line (split-string (nth 1 flymake-settings-for-file))))

  (flymake-mode)
  ;; It would be better to specify that the advice is buffer-local
  ;; so that we only advise flymake-cc in buffers that are part of a
  ;; simple project, but buffer-local advice only works for
  ;; functions that are called by storing the function symbol in a
  ;; variable and advising the variable (AFAICT), so we just have to
  ;; advise it globally and do nothing in the case when the buffer
  ;; is not related to a simple project.
  (advice-add 'flymake-cc :around #'simpleproj-flymake-cc-advice-change-wd))

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

(defun simpleproj-get-compilation-command-for-flymake ()
  simpleproj-flymake-command-line)

(defvar simpleproj-projects '()
  "Global list of simple projects")

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
  (let ((default-directory (or simpleproj-flymake-working-directory
                              default-directory)))
    (funcall orig-function report-fn args)))
