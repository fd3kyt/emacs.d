;;; init-local-autosave.el --- my setting on auto save

;;; Commentary:
;;

;;; Code:

(defvar kyt/auto-save-interval 600)

(declare-function require-package 'init-elpa)
(setq auto-save-interval kyt/auto-save-interval
      auto-save-timeout 120)

(require-package 'real-auto-save)
(require 'real-auto-save)
(setq real-auto-save-interval kyt/auto-save-interval)

;;;; org mode auto save
(defun kyt/org-setup-autosave ()
  "Setup autosave for `org-mode'."
  (setq-local real-auto-save-interval kyt/auto-save-interval)
  (real-auto-save-mode))
(add-hook 'org-mode-hook 'kyt/org-setup-autosave)

(require 'kyt-no-message)
(defun kyt/advice-around-save-buffers (fun &rest rest)
  "Will eval (apply FUN REST)."
  (save-message
   (save-excursion
     (apply fun rest))))
(advice-add 'real-auto-save-buffers :around 'kyt/advice-around-save-buffers)
;; (advice-remove 'real-auto-save-buffers 'save-message-advice-around)

;; (setq whitespace-cleanup-mode-preserve-point nil)

;; (defun try-clean-up ()
;;   (interactive)
;;   (save-mark-and-excursion)
;;   (whitespace-cleanup))

(provide 'init-local-autosave)

;;; init-local-autosave.el ends here
