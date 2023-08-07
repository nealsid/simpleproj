(setq sproj--proj-name "simpleproj")
(setq sproj--version "0.7")
(setq sproj--root-dir "/home/nealsid/git/simpleprojelisp/")
(setq sproj--description "SimpleProj is an elisp library to help manage project-related development tasks.")
(setq sproj--package-file-name "simpleproj-pkg.el")

(setq sproj--file-list
      '("simpleproj.el"
        "core/simpleproj-main.el"
        "core/simpleproj-db-init-and-load.el"
        "core/simpleproj-db-query.el"
        "core/util.el"
        "core/sproj--build-package.el"
        "features/simpleproj-cc-json-functions.el"
        "features/simpleproj-flymake.el"
        "tests/main.c"
        "tests/run-tests.sh"
        "tests/simpleproj-tests.el"
        "tests/util-test.el"))

(defun sproj--make-elisp-pkg ()
  (let* ((packaging-root-dir (concat (make-temp-file "simpleproj-packaging-" t) "/")) ;; /tmp/<tmpdirname>
         (package-relative-directory (concat sproj--proj-name "-" sproj--version "/"))
         (package-root-dir (concat packaging-root-dir package-relative-directory)) ;; /tmp/<tmpdirname>/simpleproj-<version>
         (package-tar-fn (concat sproj--proj-name "-" sproj--version ".tar"))
         ;; Generate a tar command which duplicates source files into the
         ;; temporary packaging directory, which is the easiest way I could
         ;; find to preserve relative paths (the --parents flag to cp, in
         ;; my testing, recreated the full pathname, not just the relative
         ;; path)
         (copy-tar-command (string-join (flatten-tree (list "tar cv" sproj--file-list
                                                      (concat  "| tar -xv --one-top-level=" package-root-dir))) " ")))
    (make-directory package-root-dir)
    (sproj--log "%s" copy-tar-command)
    (let ((default-directory sproj--root-dir))
      (shell-command copy-tar-command))

    ;; Create the simpleproj-pkg.el file - do this rather than
    ;; hand-maintaining it, because Emacs requires (define-package) to
    ;; be the first line of the file, which means we can't define
    ;; variables for the project/version/description to be used in the
    ;; (define-package) call.
    (with-current-buffer (find-file-noselect (concat package-root-dir sproj--package-file-name))
      (insert (concat "(define-package \"" sproj--proj-name "\" \"" sproj--version "\" \"" sproj--description "\")"))
      (save-buffer)
      (kill-buffer))
    (let ((default-directory packaging-root-dir))
      (shell-command (concat "tar cvf " package-tar-fn " " package-relative-directory)))
    (concat packaging-root-dir package-tar-fn)))

(defun sproj--rebuild-and-reinstall ()
  (interactive)
  (let ((package-filename (sproj--make-elisp-pkg))
        (package-desc (cadr (assq 'simpleproj package-alist))))
    (when package-desc
      (package-delete package-desc))
    (package-install-file package-filename)
    (message "Installed from %s" package-filename)))
