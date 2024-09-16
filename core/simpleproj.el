;;; simpleproject --- A project to make some project related tasks easier (for me)
;;;
;;; Commentary:
;;;
;;; SimpleProj tries to tie together a compilation command
;;; database with flymake, as well as some tags- and file- related
;;; management tasks.

;;; Code:

(load-file "core/simpleproj-main.el")
(load-file "core/simpleproj-db-init-and-load.el")
(load-file "core/simpleproj-db-query.el")
(load-file "core/util.el")
(load-file "features/simpleproj-cc-json-functions.el")
(load-file "features/simpleproj-flymake.el")

(when (string-equal user-login-name "nealsid")
  (load-file "core/sproj--build-package.el")
  (load-file "tests/simpleproj-tests.el")
  (load-file "tests/util-test.el"))
