;;; init-local-autosave.el --- my setting on auto save

;;; Commentary:
;;

;;; Code:

(setq auto-save-interval 300
      auto-save-timeout 120)

(require-package 'real-auto-save)
(require 'real-auto-save)
(setq real-auto-save-interval 10)

;;;; org mode auto save
(defun kyt/org-setup-autosave ()
  "Setup autosave for `org-mode'."
  (setq-local real-auto-save-interval 3)
  (real-auto-save-mode))
(add-hook 'org-mode-hook 'kyt/org-setup-autosave)

(defun kyt/clear-message ()
  "Clear the message in minibuffer."
  (message nil))
(advice-add 'real-auto-save-buffers :after 'kyt/clear-message)

(after-load 'whitespace
  (defvar whitespace-cleanup-mode-preserve-point)
  (setq whitespace-cleanup-mode-preserve-point t)) ;not working


(provide 'init-local-autosave)

;;; init-local-autosave.el ends here
