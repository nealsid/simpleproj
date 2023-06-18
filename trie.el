(defmacro precondition (forms)
  (list 'progn `(cl-loop for (precondition-form
                              error-clause)
                         on ,forms by 'cddr
                         do (cond ((not (eval precondition-form))
                                   (message "listp: %s " (listp error-clause))
                                   ;; Order of clauses to OR is
                                   ;; important because get will raise
                                   ;; an error if it's a string.
                                   (cond ((or (stringp error-clause)
                                              (and (symbolp error-clause)
                                                   (get error-clause 'error-conditions)))
                                          (error error-clause))
                                         ((listp error-clause)
                                          (apply 'error error-clause))
                                         (t (eval error-clause))))))))

(defun add-string-to-trie (trie str idx)
  (print (list trie str idx))
  (print (list "hello: " (length> str idx)))
  (precondition
   '((not (equal str nil)) ("No input string")
     (>= idx 0)            ("Invalid index: %s" idx)))

;;          (not (equal trie nil))      "Nil trie"))
  ;; Handle nil trie as a special case
  (if (equal trie nil)
      (progn
        (setq trie `(,(cons (aref str idx) nil)))
        (setf (cdr (nth 0 trie))
              (if (equal idx (- (length str) 1))
                  nil
                (add-string-to-trie nil str (+ 1 idx))))
        trie))

  ;; (let* ((ch (aref str idx))
  ;;        (trie-node (cond (trie
  ;;                          (search-or-create-trie-node trie ch))
  ;;                         (t (list (cons ch nil))))))
  ;;   (cond ((eq idx (- (length str) 1))
  ;;          trie-node)
  ;;         (t (add-string-to-trie (cdr trie-node) str (+ idx 1))))
  ;;   trie)
  )

(create-or-update-trie nil (list "aaa" "bbb"))
(("b" . nextlevel) ("a" . nextlevel))

(setq test-trie `(,(cons ?a 'nextlevel) ,(cons ?b 'nextlevel2)))
(search-or-create-trie-node test-trie ?c)

(macroexpand '(setf test-trie (cons (cons 'a 'b) test-trie)))

(setq test-trie (cons (cons 'a 'b) test-trie))

(defun search-or-create-trie-node (trie char)
  (or (cdr (cl-find-if (lambda (cell)
                    (equal (car cell) char))
                  trie))
      (progn
        (let ((newnode (cons char "foo")))
          (setcdr trie (list newnode (cdr trie)))
          (cdr newnode)))))
