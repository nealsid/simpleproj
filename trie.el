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

(defun make-trie-node (ch terminal-p next-trie-level user-data)
  (list ch terminal-p next-trie-level user-data))

(defun add-string-to-trie (trie str entry-data str-idx)
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
        (setq trie (list (cons (aref str str-idx)
                               (make-trie-entry-data nil nil nil))))
        (setq trie-node-entry (nth 0 trie)))
    (setq trie-node-entry (search-or-create-trie-node-entry trie (aref str str-idx)))
    (cl-assert (consp trie-node-entry))
    (cl-assert (member trie-node-entry trie)))

  (cond ((equal str-idx (1- (length str)))
         (set-trie-entry-terminal trie-node-entry t)
         (set-trie-entry-user-data trie-node-entry entry-data)
         trie)
        (t (progn
             (setf (caddr trie-node-entry) ;; TODO write accessors for
                                           ;; trie data rather than
                                           ;; using c*r functions
                                           ;; directly.
                   (add-string-to-trie (caddr trie-node-entry) str entry-data (1+ str-idx)))
             trie))))

(defun search-or-create-trie-node-entry (trie char)
  (or (cl-find-if (lambda (cell)
                    (equal (car cell) char))
                  trie)
      (progn
        (let ((new-trie-node-entry (cons char
                                         (make-trie-entry-data nil nil nil))))
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

(defun lookup-string-recursive (trie lookup-string idx)
  (let ((trie-lookup (cl-find-if (lambda (cell)
                                   (equal (car cell) (aref lookup-string idx)))
                                 trie)))
    (cond (trie-lookup
           (cond ((and (equal idx (1- (length lookup-string)))
                       (trie-entry-terminal-p (cdr trie-lookup)))
                  (or (trie-entry-data-user-data (cdr trie-lookup))
                      t))
                 ((equal idx (1- (length lookup-string)))
                  nil)
                 (t (lookup-string-recursive (caddr trie-lookup) lookup-string (1+ idx)))))
          (t nil))))

(defun lookup-string (trie lookup-string)
  (lookup-string-recursive trie lookup-string 0))

(defun make-trie-entry-data (terminal next-level-trie user-data)
  (precondition
   `((booleanp terminal) ("Terminal is not boolean")))

  (list terminal next-level-trie user-data))

(defun set-trie-entry-terminal (trie-entry-data is-terminal)
  (setf (cadr trie-entry-data) is-terminal))

(defun set-trie-entry-user-data (trie-entry-data user-data)
  (setf (cadddr trie-entry-data) user-data))

(defun trie-entry-terminal-p (trie-entry-data)
  (car trie-entry-data))

(defun trie-entry-data-next-level (trie-entry-data)
  (cadr trie-entry-data))

(defun trie-entry-data-user-data (trie-entry-data)
  (caddr trie-entry-data))

(defun map-trie-strings (trie fn-to-call current-string)
  (mapc (lambda (trie-node-entry)
          (cond ((equal (caddr trie-node-entry) nil)
                 (apply fn-to-call (list (concat current-string (char-to-string (car trie-node-entry))))))
                (t (map-trie-strings (caddr trie-node-entry) fn-to-call (concat current-string (char-to-string (car trie-node-entry)))))))
        trie)
  nil)
