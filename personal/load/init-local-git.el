;;; init-local-git.el --- Initialize git, magit for me.

;;; Commentary:
;;

;;; Code:
(declare-function 'require-package 'init-elpa)
(declare-function 'after-load 'subr)

(after-load 'magit
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; https://github.com/magit/magit/issues/1275
(defadvice magit-start-git (around lang-en_US activate)
  "Set LANG to en_US."
  (let ((process-environment process-environment))
    (setenv "LANG" "en_US")
    ad-do-it))

(require-package 'magit-gitflow)

(after-load 'magit
  (require 'magit-gitflow)
  (defvar magit-gitflow-mode-map)
  (define-key magit-gitflow-mode-map (kbd "C-f") nil)
  (define-key magit-gitflow-mode-map (kbd "C-c f") 'magit-gitflow-popup)
  (defvar magit-log-margin)
  (setq magit-log-margin '(t "%y-%m-%d %H:%M" magit-log-margin-width t 18)))

(provide 'init-local-git)

;;; init-local-git.el ends here
