(setq sproj--proj-name "simpleproj")
(setq sproj--version "0.7")
(setq sproj--root-dir "/home/nealsid/git/simpleprojelisp/")
(setq sproj--description "SimpleProj is an elisp library to help manage project-related development tasks.")
(setq sproj--package-file-name "simpleproj-pkg.el")

(setq sproj--file-list
      '("core/simpleproj.el"
        "core/simpleproj-db-init-and-load.el"
        "core/simpleproj-db-query.el"
        "core/util.el"
        "features/simpleproj-cc-json-functions.el"
        "features/simpleproj-flymake.el"
        "tests/main.c"
        "tests/run-tests.sh"
        "tests/simpleproj-tests.el"
        "tests/util-test.el"))

(defun sproj--make-elisp-pkg ()
  (let* ((packaging-root-dir (concat (make-temp-file "simpleproj-packaging-" t) "/"))
         (package-relative-directory (concat sproj--proj-name "-" sproj--version "/"))
         (package-root-dir (concat packaging-root-dir package-relative-directory))
         (package-tar-fn (concat sproj--proj-name "-" sproj--version ".tar"))
         (default-directory packaging-root-dir))
    (make-directory package-root-dir)
    (mapc (lambda (fn)
            (copy-file (concat sproj--root-dir fn) (concat package-root-dir (file-name-nondirectory fn))))
          sproj--file-list)
    (with-current-buffer (find-file-noselect (concat package-root-dir sproj--package-file-name))
      (insert (concat "(define-package \"" sproj--proj-name "\" \"" sproj--version "\" \"" sproj--description "\")"))
      (save-buffer)
      (kill-buffer))
    (shell-command (concat "tar cvf " package-tar-fn " " package-relative-directory))
    (concat packaging-root-dir package-tar-fn)))

(defun sproj--rebuild-and-reinstall ()
  (interactive)
  (let ((package-filename (sproj--make-elisp-pkg))
        (package-desc (cadr (assq 'simpleproj package-alist))))
    (when package-desc
      (package-delete package-desc))
    (package-install-file package-filename)
    (message "Installed from %s" package-filename)))
