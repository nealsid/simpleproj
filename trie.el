(defmacro precondition (forms)
  (list 'progn `(cl-loop for (precondition-form
                              error-clause)
                         on ,forms by 'cddr
                         do (cond ((not (eval precondition-form)) (error error-clause))))))

(defun add-string-to-trie (trie str idx)
  (precondition
   '((not (equal trie nil)) "Trie is nil"
     (> idx 0)              "Invalid index"))
  (let* ((ch (aref str idx))
         (trie-node (cond (trie
                           (search-or-create-trie-node trie ch))
                          (t (list (cons ch nil))))))
    (cond ((eq idx (- (length str) 1))
           trie-node)
          (t (add-string-to-trie (cdr trie-node) str (+ idx 1))))
    trie-node))

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
