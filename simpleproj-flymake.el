;;; simpleproject-flymake --- Code containing flymake integration of SimpleProj
;;;
;;; Commentary:
;;;
;;; This file contains functions necessary for initializing FlyMake
;;; when visiting a file that is part of a SimpleProj project which
;;; uses FlyMake.

;;; Code:
(require 'flymake)

(defun simpleproj-get-compilation-command-for-flymake ()
  simpleproj-flymake-command-line)

(defun simpleproj-flymake-cc-advice-change-wd (orig-function report-fn &rest args)
  "Function meant to be used as advice for `flymake-cc'.  Change the
working directory to what is specified in compile_commands.json
before invoking `flymake-cc'.  If the current buffer is not part
of a simple project, just call (ORIG-FUNCTION REPORT-FN ARGS)
with no change in environment."
  (let ((default-directory (or simpleproj-flymake-working-directory
                              default-directory)))
    (funcall orig-function report-fn args)))

(defun simpleproj-configure-flymake ()
  "Function meant to be called during `simpleproj-minor-mode'
initialization.  Turns on flymake and advises `flymake-cc' in
order to change the working directory while the compiler is being
invoked."
  (let ((flymake-settings-for-file
         (simpleproj-query-for-file-flymake-settings
          simpleproj-project
          (buffer-file-name))))
    (if (not flymake-settings-for-file)
        nil
      (defvar-local simpleproj-flymake-command-line nil
        "Buffer-local variable referring to the Flymake command line for this buffer")
      (defvar-local simpleproj-flymake-working-directory nil
        "Buffer-local variable referring to the Flymake command working directory for this buffer")
      (setq simpleproj-flymake-working-directory (nth 0 flymake-settings-for-file))
      (setq simpleproj-flymake-command-line (split-string (nth 1 flymake-settings-for-file))))

    ;; There should probably be some sanity check in case this
    ;; variable is overwritten by something unrelated to SimpleProj.
    (setq flymake-cc-command
          'simpleproj-get-compilation-command-for-flymake)

    ;; It would be better to specify that the advice is buffer-local
    ;; so that we only advise flymake-cc in buffers that are part of a
    ;; simple project, but buffer-local advice only works for
    ;; functions that are called by storing the function symbol in a
    ;; variable and advising the variable (AFAICT), so we just have to
    ;; advise it globally and do nothing in the case when the buffer
    ;; is not related to a simple project.
    (advice-add 'flymake-cc :around #'simpleproj-flymake-cc-advice-change-wd)
    (flymake-mode)))
