;;; kyt-avy.el --- Works upon avy.el

;;; Commentary:
;; Useful ag.el functions
;; avy--line avy--generic-jump

;;; Code:


(require 'avy)
(defun kyt/avy-copy-line-here (arg)
  "Copy a selected line to current position.
ARG lines can be used."
  (interactive "p")
  (let ((initial-window (selected-window)))
    (avy-with avy-copy-line
      (let* ((start (avy--line))
             (str (buffer-substring-no-properties
                   start
                   (save-excursion
                     (goto-char start)
                     (move-end-of-line arg)
                     (point)))))
        (select-window initial-window)
        (insert (if (> arg 1)
                    str
                  (s-trim str)))))))


(provide 'kyt-avy)

;;; kyt-avy.el ends here
