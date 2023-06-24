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

(defun make-trie-node-entry (char child-trie)
  (precondition
   `((characterp char) ("Wrong type, characterp")))
  (cons char child-trie))

(defun make-trie-node ()
  nil)

(defun add-trie-node-entry-to-trie-node (trie node-entry)
  (cond ((eq trie nil)
         (set trie (list node-entry)))
        (t (setf (cdr trie) (cons node-entry (cdr trie))))))

(defun add-string-to-trie (trie str str-idx)
  (precondition
   `((not (equal str nil))        ("No input string")
     (and (>= str-idx 0)
          (length> str str-idx)) ,(list "Invalid index: %s" str-idx)
     (or (equal trie nil)
         (symbolp trie)
         (listp trie))           ,(list "Trie arg has invalid type: %s" (type-of trie))))

  ;; Handle nil trie as a special case
  (if (equal trie nil)
      (progn
        (setq trie (list (cons (aref str str-idx) nil)))
        (setq trie-node-entry (nth 0 trie)))
    (setq trie-node-entry (search-or-create-trie-node-entry trie (aref str str-idx)))
    (cl-assert (consp trie-node-entry))
    (cl-assert (member trie-node-entry trie)))

  (cond ((equal str-idx (1- (length str)))
         trie)
        (t (progn
             (setf (cdr trie-node-entry) (add-string-to-trie (cdr trie-node-entry) str (1+ str-idx)))
             trie))))


(defun search-or-create-trie-node-entry (trie char)
  (or (cl-find-if (lambda (cell)
                    (equal (car cell) char))
                  trie)
      (progn
        (let ((new-trie-node-entry (cons char nil)))
          (setf (cdr trie) (cond ((equal (cdr trie) nil)
                                  ;; If the cdr is nil, we can return
                                  ;; the entry as a single-element list
                                  (list new-trie-node-entry))
                                 ;; If the cdr is not nil, we have to
                                 ;; flatten the cdr so that list does
                                 ;; not create an element containing a
                                 ;; list, so use apply. I tried using
                                 ;; append, as well, but that flattens
                                 ;; the new-trie-node-entry cons cell.
                                 (t (apply 'list new-trie-node-entry (cdr trie)))))
          new-trie-node-entry))))

(defun print-trie-strings (trie)
  (mapcar (lambda (trie-node-entry)
            (cond ((characterp (car trie-node-entry))
                   (princ (char-to-string (car trie-node-entry))))
                  (t (princ (car trie-node-entry))))
            (cond ((not (equal (cdr trie-node-entry) nil))
                   (print-trie-strings (cdr trie-node-entry)))
                  (t (princ "\n"))))
          trie))

(defun map-trie-strings (trie fn-to-call current-string)
  (mapc (lambda (trie-node-entry)
          (cond ((equal (cdr trie-node-entry) nil)
                 (apply fn-to-call (list (concat current-string (char-to-string (car trie-node-entry))))))
                (t (map-trie-strings (cdr trie-node-entry) fn-to-call (concat current-string (char-to-string (car trie-node-entry)))))))
        trie)
  nil)
