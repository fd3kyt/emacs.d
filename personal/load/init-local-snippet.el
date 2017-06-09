;;; init-local-snippet.el --- Initialize yasnippet for me.

;;; Commentary:
;;

;;; Code:

(defun extract-snippet-name ()
  "Extract the snippet name from a new snippet buffer."
  (let ((name-line-start "# name: "))
    (let ((match-lines (cl-remove-if-not
                        (lambda (line)
                          (s-starts-with-p name-line-start line))
                        (s-lines (buffer-string)))))
      (if match-lines
          (let ((name (s-chop-prefix name-line-start (car match-lines))))
            name)
        (message "Can't find snippet name field!")))))

(defun rename-new-snippet-buffer ()
  "Rename new snippet buffer with the snippet name."
  (rename-buffer (s-replace " " "_" (extract-snippet-name))))

(defun kyt/save-file-rename-if-new ()
  "Rename current new snippet buffer and save it."
  (interactive)
  (unless (buffer-name)
    (rename-new-snippet-buffer))
  (save-buffer))

(after-load 'yasnippet
  (define-key snippet-mode-map (kbd "C-x C-s")
    'kyt/save-file-rename-if-new))

(provide 'init-local-snippet)

;;; init-local-snippet.el ends here
