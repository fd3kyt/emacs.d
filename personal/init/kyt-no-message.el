;;; kyt-no-message.el --- Summary

;;; Commentary:
;; utilities for suppressing message.
;;
;; why not just use `inhibit-message': will clear current message

;;; Code:

(defun clear-message ()
  "Clear the message in minibuffer."
  (message nil))

(defmacro no-message (&rest body)
  "Eval BODY, with `message' doing nothing."
  `(cl-letf (((symbol-function 'message)
              (lambda (&rest args)
                nil)))
     (progn ,@body)))

(defmacro save-message (&rest body)
  "Save the message in minibuffer; execute BODY; restore the message."
  (let ((saved-message-symbol (make-symbol "saved-message")))
    `(let ((,saved-message-symbol (current-message))
           (return (progn ,@body)))
       (message ,saved-message-symbol)
       return)))

(defun save-message-advice-around (oldfun rest)
  "Eval (OLDFUN REST) in `save-message'.
Usage: (advice-add 'real-auto-save-buffers :around 'save-message-advice-around)"
  (save-message
   (apply oldfun rest)))

(defun --try-out-save-message ()
  "See how `save-message' works out."
  (interactive)
  (message (format (propertize "The message I am staring at (with property)."
                               'face
                               'warning)))
  (save-message
   (message "Message from the auto save function.")))

(defun --try-inhibit-message ()
  "Try to use `inhibit-message'."
  (interactive)
  (message "Old message")
  (let ((inhibit-message t))
    ;; will still clear current message
    (message "New message")))

(provide 'kyt-no-message)

;;; kyt-no-message.el ends here
