;;; simpleproject --- A project to make some project related tasks easier (for;;; simpleproject --- A project to make some project related tasks easier (for me)
;;;
;;; Commentary:
;;;
;;; SimpleProj tries to tie together a compilation command
;;; database with flymake, as well as some tags- and file- related
;;; management tasks.

;;; Code:

(setq sproj--files-to-load
      '("core/simpleproj-main.el"
        "core/simpleproj-db-init-and-load.el"
        "core/simpleproj-db-query.el"
        "core/util.el"
        "features/simpleproj-cc-json-functions.el"
        "features/simpleproj-flymake.el"))

(setq sproj--dev-files
      '("core/sproj--build-package.el"
        "tests/simpleproj-tests.el"
        "tests/util-test.el"))

(let ((package-path (file-name-directory load-file-name)))
  (mapc 'load-file
        (mapcar (lambda (fn)
                  (concat package-path fn)) sproj--files-to-load))
  (when (string-equal user-login-name "nealsid")
    (mapc 'load-file
          (mapcar (lambda (fn)
                    (concat package-path fn)) sproj--dev-files))))
