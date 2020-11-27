;;; init-local-git.el --- Initialize git, magit for me.

;;; Commentary:
;;

;;; Code:
(declare-function 'require-package 'init-elpa)
(declare-function 'after-load 'subr)

;; https://github.com/magit/magit/issues/1275
(defadvice magit-start-git (around lang-en_US activate)
  "Set LANG to en_US."
  (let ((process-environment process-environment))
    (setenv "LANG" "en_US")
    ad-do-it))

(after-load 'magit
  ;; update the highlights of uncommitted changes in fridge
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  (defvar magit-log-margin)
  (setq magit-log-margin '(t "%y-%m-%d %H:%M" magit-log-margin-width t 18))

  (defvar magit-status-mode-map)
  (define-key magit-status-mode-map (kbd "C-c 1") 'magit-section-show-level-1-all)
  (define-key magit-status-mode-map (kbd "C-c 2") 'magit-section-show-level-2-all)
  (define-key magit-status-mode-map (kbd "C-c 3") 'magit-section-show-level-3-all)
  (define-key magit-status-mode-map (kbd "C-c 4") 'magit-section-show-level-4-all)
  )


;;; magit-gitflow
(require-package 'magit-gitflow)
(after-load 'magit
  (require 'magit-gitflow)
  (defvar magit-gitflow-mode-map)
  (define-key magit-gitflow-mode-map (kbd "C-f") nil)
  (define-key magit-gitflow-mode-map (kbd "C-c f") 'magit-gitflow-popup))

(provide 'init-local-git)

;;; init-local-git.el ends here
