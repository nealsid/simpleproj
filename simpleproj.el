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
  (filename-to-compile-command-ht nil :documentation "Hash table of filename to compilation command"))


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

(add-hook 'simpleproj-minor-mode-hook 'simpleproj-build-compilation-hash-hook)
(add-hook 'simpleproj-minor-mode-hook 'simpleproj-add-flymake-advice)

(defun simpleproj-add-flymake-advice ()

  )

(defun simpleproj-build-compilation-hash-hook ()
  (and simpleproj-minor-mode
       (let ((sproj (simpleproj-find-matching-project-for-buffer)))
         (when (simpleproj-compilation-command-json-exists-p sproj)
           (setf (simple-project-filename-to-compile-command-ht sproj)
                 (simpleproj-build-compilation-hash
                  (make-compile-commands-json (concat (simple-project-build-root sproj) "/compile_commands.json"))))))))

(cl-defun add-simple-project (&key project-name
                                   project-short-name
                                   source-root
                                   build-root
                                   compile-commands-command
                                   filename-to-compile-commands-ht)
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

(setq filename-trie ())

(defun simpleproj-build-compilation-hash (json)
  (let ((filename-to-compilation-command (make-hash-table :test 'equal :size (length json))))
    (mapc (lambda (x)
            (let ((file-full-path (gethash "file" x))
                  (file-compilation-command (gethash "command" x)))
              (puthash file-full-path file-compilation-command filename-to-compilation-command)
              (setq filename-trie (add-string-to-trie filename-trie file-full-path 0))))
          json)
    filename-to-compilation-command))

(defun simpleproj-get-compilation-command (sproj-project)
  (gethash (buffer-file-name) (simple-project-filename-to-compile-command-ht sproj-project)))

(defun compile-using-project-compilation-command ()
  (interactive)
  (let* ((project (simpleproj-find-matching-project-for-buffer))
         (compiler-command (simpleproj-get-compilation-command project))
         (full-compile-command (concat "cd " (simple-project-build-root project) " && " compiler-command)))
    (compile full-compile-command)))

(defun file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun make-compile-commands-json (pathname)
  (json-parse-string (file-to-string pathname)))

(defun simpleproj-flycheck-command-args-list ()
  (list "gcc"
        "-Wp,-MMD,drivers/net/wireless/broadcom/brcm80211/brcmfmac/.chip.o.d"
        "-nostdinc"
        "-I./arch/x86/include"
        "-I./arch/x86/include/generated"
        "-I./include"
        "-I./arch/x86/include/uapi"
        "-I./arch/x86/include/generated/uapi"
        "-I./include/uapi"
        "-I./include/generated/uapi"
        "-include"
        "./include/linux/compiler-version.h"
        "-include"
        "./include/linux/kconfig.h"
        "-include"
        "./include/linux/compiler_types.h"
        "-D__KERNEL__"
        "-fmacro-prefix-map=./="
        "-Wall"
        "-Wundef"
        "-Werror=strict-prototypes"
        "-Wno-trigraphs"
        "-fno-strict-aliasing"
        "-fno-common"
        "-fshort-wchar"
        "-fno-PIE"
        "-Werror=implicit-function-declaration"
        "-Werror=implicit-int"
        "-Werror=return-type"
        "-Wno-format-security"
        "-funsigned-char"
        "-std=gnu11"
        "-mno-sse"
        "-mno-mmx"
        "-mno-sse2"
        "-mno-3dnow"
        "-mno-avx"
        "-fcf-protection=branch"
        "-fno-jump-tables"
        "-m64"
        "-falign-jumps=1"
        "-falign-loops=1"
        "-mno-80387"
        "-mno-fp-ret-in-387"
        "-mpreferred-stack-boundary=3"
        "-mskip-rax-setup"
        "-mtune=generic"
        "-mno-red-zone"
        "-mcmodel=kernel"
        "-Wno-sign-compare"
        "-fno-asynchronous-unwind-tables"
        "-mindirect-branch=thunk-extern"
        "-mindirect-branch-register"
        "-mindirect-branch-cs-prefix"
        "-mfunction-return=thunk-extern"
        "-fno-jump-tables"
        "-mharden-sls=all"
        "-fpatchable-function-entry=16,16"
        "-fno-delete-null-pointer-checks"
        "-Wno-frame-address"
        "-Wno-format-truncation"
        "-Wno-format-overflow"
        "-Wno-address-of-packed-member"
        "-O2"
        "-fno-allow-store-data-races"
        "-Wframe-larger-than=2048"
        "-fstack-protector-strong"
        "-Wno-main"
        "-Wno-unused-but-set-variable"
        "-Wno-unused-const-variable"
        "-Wno-dangling-pointer"
        "-ftrivial-auto-var-init=zero"
        "-fno-stack-clash-protection"
        "-pg"
        "-mrecord-mcount"
        "-mfentry"
        "-DCC_USING_FENTRY"
        "-falign-functions=16"
        "-Wdeclaration-after-statement"
        "-Wvla"
        "-Wno-pointer-sign"
        "-Wcast-function-type"
        "-Wno-stringop-truncation"
        "-Wno-stringop-overflow"
        "-Wno-restrict"
        "-Wno-maybe-uninitialized"
        "-Wno-array-bounds"
        "-Wno-alloc-size-larger-than"
        "-Wimplicit-fallthrough=5"
        "-fno-strict-overflow"
        "-fno-stack-check"
        "-fconserve-stack"
        "-Werror=date-time"
        "-Werror=incompatible-pointer-types"
        "-Werror=designated-init"
        "-Wno-packed-not-aligned"
        "-g"
        "-gdwarf-5"
        "-DDEBUG"
        "-I./drivers/net/wireless/broadcom/brcm80211/brcmfmac"
        "-I./drivers/net/wireless/broadcom/brcm80211/brcmfmac/../include"
        "-DMODULE"
        "-DKBUILD_BASENAME='\"chip\"'"
        "-DKBUILD_MODNAME='\"brcmfmac\"'"
        "-D__KBUILD_MODNAME=kmod_brcmfmac"
        "-c" (buffer-file-name)))
