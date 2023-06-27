(let ((gc-cons-threshold 1000000000))
  (benchmark 10 '(make-compile-commands-json (concat (simple-project-build-root (nth 0 simpleproj-projects)) "/compile_commands.json"))))

g
(let ((return-results (make-list 4 0)))
  (dotimes (i 4)
    (make-thread (lambda ()
                     (message "hello"))))
                   (make-compile-commands-json (concat (simple-project-build-root (nth 0 simpleproj-projects)) "/compile_commands-1.json"))))))


(all-threads)

 (make-compile-commands-json (concat (simple-project-build-root (nth 0 simpleproj-projects)) "/compile_commands-" (1+ i) ".json"))
(defun update-chunk-positions-to-be-on-json-record-bounaries (chunk-positions)
  (mapcar (lambda (position)
            (goto-char position)
            (re-search-forward "}\\|{")
            (point))
          chunk-positions))

(defun parse-json-in-chunks (json-file-path number-chunks)
  (with-temp-buffer
    (insert-file-contents-literally json-file-path)
    (let ((chunk-size (/ (buffer-size) number-chunks))
          (chunk-positions '()))
      (dotimes (i number-chunks)
        (push (* i chunk-size) chunk-positions))
      (update-chunk-positions-to-be-on-json-record-bounaries chunk-positions))))

(defun divide-json-file-into-equal-size-chunks (json-file-path number-chunks)
  (with-temp-buffer
    (insert-file-contents-literally json-file-path)
    (let ((chunk-size (/ (buffer-size) number-chunks))
          (chunk-positions '()))
      (dotimes (i number-chunks)
        (push (* i chunk-size) chunk-positions))
      chunk-positions)))


divide-json-file-into-equal-size-chunks
(divide-json-file-into-equal-size-chunks "/home/nealsid/git/linux/compile_commands.json" 4)
(13879398 9252932 4626466 0)
(parse-json-in-chunks "/home/nealsid/git/linux/compile_commands.json" 4)
(13879536 9253335 4627164 6)
(13879536 9253335 4627164 2222)
(with-current-buffer "compile_commands.json"
  (json-parse-string (buffer-substring-no-properties 1 4627165)))
(13879398 9252932 4626466 0)
