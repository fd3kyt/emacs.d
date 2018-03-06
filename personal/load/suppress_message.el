;;; suppress_message.el --- Summary

;;; Commentary:
;;

;;; Code:



(defmacro no-message (&rest body)
  "Eval BODY, with `message' doing nothing."
  `(cl-letf (((symbol-function 'message)
              (lambda (&rest args)
                nil)))
     (progn ,@body)))

(no-message
 (message "haha")
 (print [1 2 3])
 (message "good morning")
 (save-buffer))

(defun try-current-message ()
  "Try."
  (interactive)
  (let ((m (current-message)))
    (message "in try-current-message")
    (message m)))

(local-set-key (kbd "C-c k") 'try-current-message)


(defmacro save-message (&rest body)
  "Eval BODY, keeping the message in minibuffer untouched."
  `(let ((saved-message (current-message)))
     (progn ,@body)
     (message saved-message)))


(provide 'suppress_message)

;;; suppress_message.el ends here
