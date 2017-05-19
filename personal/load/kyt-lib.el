;;; kyt-lib.el --- Util for kyt

;;; Commentary:
;;

;;; Code:

(defun kyt/get-reasonable-file-name (string)
  "Replace special characters in STRING, make it a valid file name."
  (replace-regexp-in-string "[: *&^%$#@!?\"'/]" "_"
                            string))

(defcustom kyt/temp-file-base-name  (concat (substitute-in-file-name "$HOME/")
                                            "emacs-temp/")
  "Default file base name of kyt/buffer-file-name-or-default."
  :type 'string
  :group 'kyt-lib)

(defun kyt/buffer-file-name-or-default ()
  "Return `w/buffer-file-name'.
If it is nil, a make temp name with kyt/temp-file-base-name as base name."
  (unless (buffer-file-name)
    (setq-local buffer-file-name
                (make-temp-name kyt/temp-file-base-name)))
  (buffer-file-name))

;;; vertical editing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kyt/move-to-goal-column ()
  "Move to `goal-column' if it is setted, add space if line is too short."
  (if goal-column
      (move-to-column goal-column t)))

(defun kyt/goal-column-next-line (&optional ARG TRY-VSCROLL)
  "Next line and `kyt/move-to-goal-column'.
Pass `ARG' and `TRY-VSCROLL' to `next-line'."
  (interactive "P")
  (next-line ARG TRY-VSCROLL)
  (kyt/move-to-goal-column))

(defun kyt/goal-column-previous-line (&optional ARG TRY-VSCROLL)
  "Previous line and `kyt/move-to-goal-column'.
Pass `ARG' and `TRY-VSCROLL' to `previous-line'."
  (interactive "P")
  (previous-line ARG TRY-VSCROLL)
  (kyt/move-to-goal-column))

(define-minor-mode kyt/vertical-editing
  "Get your foos in the right places."
  :lighter " vert"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-n")
              'kyt/goal-column-next-line)
            (define-key map (kbd "C-p")
              'kyt/goal-column-previous-line)
            map))
;;; end of vertical editing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'kyt-lib)

;;; kyt-lib.el ends here
