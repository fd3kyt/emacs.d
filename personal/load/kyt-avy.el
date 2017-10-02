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


(declare-function org-insert-link 'org)
(defvar org-stored-links)
(defun kyt/avy-insert-line-link ()
  "Insert a link to a selected line at current position."
  (interactive)
  (let ((initial-window (selected-window)))
    (avy-with avy-copy-line
      (let ((start (avy--line)))
        (save-excursion
          (goto-char start)
          (call-interactively 'org-store-link)
          (select-window initial-window)
          ;; `org-insert-link' will delete the link from `org-stored-links'
          (org-insert-link nil (car (car org-stored-links))))))))


(provide 'kyt-avy)

;;; kyt-avy.el ends here
