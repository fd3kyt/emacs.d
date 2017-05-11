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


(defun kyt/artist-mode-use-origin-next-line (arg)
  "Use old `next-line', `previous-line' instead.

Usually used with `set-goal-column'.

ARG: prefix argument.  If not nil, reset to `artist-next-line'
and `artist-previous-line'."
  (interactive "P")
  (if arg
      (progn (define-key artist-mode-map [remap artist-next-line] nil)
             (define-key artist-mode-map [remap artist-previous-line] nil))
    (progn (define-key artist-mode-map [remap artist-next-line] 'next-line)
           (define-key artist-mode-map [remap artist-previous-line] 'previous-line)))
  )

(define-minor-mode kyt/vertical-editing
  "Get your foos in the right places."
  :lighter " vert"
  (artist-mode (if kyt/vertical-editing 1 -1))
  (kyt/artist-mode-use-origin-next-line kyt/vertical-editing)
  )

(defun kyt/move-to-goal-column ()
  "Move to `goal-column' if it is setted, add space if line is too short."
  (if goal-column
      (move-to-column goal-column t)))

;; TODO no, it don't work as I think. next-line won't add space.
;; I think I need to use artist-next-line and read goal-column



(provide 'kyt-lib)

;;; kyt-lib.el ends here
