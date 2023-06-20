(defmacro precondition (forms)
  (list 'progn `(cl-loop for (precondition-form
                              error-clause)
                         on ,forms by 'cddr
                         do (cond ((not (eval precondition-form))
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

(defun add-string-to-trie (trie str str-idx)
  (precondition
   `((not (equal str nil))                    ("No input string")
     (and (>= str-idx 0)
          (>= (length str) (1+ str-idx))) ,(list "Invalid index: %s" str-idx)))

  ;; Handle nil trie as a special case
  (if (equal trie nil)
      (progn
        (setq trie `(,(cons (aref str str-idx) nil)))
        (setq node (nth 0 trie)))
    (setq node (search-or-create-trie-node trie (aref str str-idx))))

  (cond ((equal str-idx (1- (length str)))
         trie)
        (t (progn
             (setf (cdr node) (add-string-to-trie (cdr node) str (1+ str-idx)))
             trie))))


(defun search-or-create-trie-node (trie char)
  (or (cl-find-if (lambda (cell)
                    (equal (car cell) char))
                  trie)
      (progn
        (let ((newnode (cons char nil)))
          (setcdr trie (list newnode (cdr trie)))
          newnode))))

(create-or-update-trie nil (list "aaa" "bbb"))
(("b" . nextlevel) ("a" . nextlevel))

(setq test-trie `(,(cons ?a 'nextlevel) ,(cons ?b 'nextlevel2)))
(search-or-create-trie-node test-trie ?c)

(macroexpand '(setf test-trie (cons (cons 'a 'b) test-trie)))

(setq test-trie (cons (cons 'a 'b) test-trie))
