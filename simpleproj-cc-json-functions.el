(defun file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun make-compile-commands-json (pathname)
  (json-parse-string (file-to-string pathname)))
