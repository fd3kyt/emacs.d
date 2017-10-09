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
  "Save the message in minibuffer; execute BODY; restore the message."
  (let ((saved-message-symbol (make-symbol "saved-message")))
    `(let ((,saved-message-symbol (current-message)))
       (progn ,@body)
       (message ,saved-message-symbol))))

(defun save-message-advice-around (oldfun &rest r)
  (save-message
   (apply oldfun r)))

(advice-add 'real-auto-save-buffers :around 'save-message-advice-around)
(advice-remove 'real-auto-save-buffers 'kyt/clear-message)

(progn
  (message "1")
  (save-message
   (message "hello")))


(defun try-property ()
  (interactive)
  (message (format (propertize "No results"
                               'face
                               'font-lock-warning-face)))
  (save-message
   (message "haha")))


(defmacro save-message (&rest body)
  "Execute a simple for loop: (for i from 1 to 10 do (print i))."
  (let ((tempvar (make-symbol "max")))
    `(let ((,tempvar (current-message)))
       (while (<= ,var ,tempvar)
         ,@body
         (inc ,var)))))



;; not working on


(provide 'suppress_message)

;;; suppress_message.el ends here
