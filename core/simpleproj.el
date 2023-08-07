;;; simpleproject --- A project to make some project related tasks easier (for me)
;;;
;;; Commentary:
;;;
;;; SimpleProj tries to tie together a compilation command
;;; database with flymake, as well as some tags- and file- related
;;; management tasks.

;;; Code:

(load-file "simpleproj-main.el")
(load-file "simpleproj-db-init-and-load.el")
(load-file "simpleproj-db-query.el")
(load-file "simpleproj-main.el")
(load-file "simpleproj.el")
(load-file "sproj--build-package.el")
(load-file "util.el")
(load-file "../features/simpleproj-cc-json-functions.el")
(load-file "../features/simpleproj-flymake.el")
;; (load-file "../tests/simpleproj-tests.el")
;; (load-file "../tests/util-test.el")
