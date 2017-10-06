;;; init-local-autosave.el --- my setting on auto save

;;; Commentary:
;;

(setq auto-save-interval 300
      auto-save-timeout 120)

;;; Code:

(require-package 'real-auto-save)
(require 'real-auto-save)
(setq real-auto-save-interval 10)

;;;; org mode auto save
(defun kyt/org-setup-autosave ()
  (setq-local real-auto-save-interval 3)
  (real-auto-save-mode))
(add-hook 'org-mode-hook 'kyt/org-setup-autosave)

(defun kyt/clear-message ()
  (message nil))
(advice-add 'real-auto-save-buffers :after 'kyt/clear-message)


(provide 'init-local-autosave)

;;; init-local-autosave.el ends here
