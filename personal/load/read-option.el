;;; read-option.el --- Summary

;;; Commentary:
;;

;;; Code:



(defun get-help-option-lines (help-command)
  "Get the option lines showed when running HELP-COMMAND."
  (let ((help-output (shell-command-to-string help-command)))
    (-filter (lambda (line) (s-matches? "^-+[[:alnum:]]" line))
             (-map 's-trim (s-lines help-output)))))

(get-help-option-lines "ag -h")

;; problem: if the info has multiple lines.


(provide 'read-option)

;;; read-option.el ends here
