(ert-deftest trie-verify-trie-against-linux-kernel-compile-commands-json ()
  "Verifies that inserting and retrieving all filenames in a linux kernel compile_commands json yields all filenames"
  (let ((filename-list (list))
        (filename-trie (add-string-to-trie nil " " 0)))
    (with-temp-buffer
      (insert-file-contents "testdata/linux-kernel-file-list.txt.bz2")
      (goto-char (point-min))
      (while (not (eobp))
        (re-search-forward "[^\n]+")
        (push (match-string 0) filename-list)
        (add-string-to-trie filename-trie (match-string 0) 0)
        (forward-line 1)))
    (mapcar (lambda (fn)
              (should (equal (lookup-string filename-trie fn) t)))
            filename-list)))

(ert-deftest trie-verify-prefixes-are-found ()
  "Verifies that a trie that contains a string that is the prefix of another string in the trie is found."
  (let ((test-trie (add-string-to-trie nil " " 0)))
    (add-string-to-trie test-trie "/main.c" 0)
    (add-string-to-trie test-trie "/main" 0)
    (add-string-to-trie test-trie "/version.c" 0)
    (should (equal
             (lookup-string test-trie "/main.c")
             t))
    (should (equal
             (lookup-string test-trie "/main")
             t))
    (should (equal
             (lookup-string test-trie "/main.d")
             nil))
    (should (equal
             (lookup-string test-trie "/main.")
             nil))))
