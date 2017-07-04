;;; init-local-git.el --- Initialize git, magit for me.

;;; Commentary:
;;

;;; Code:

(after-load 'magit
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; https://github.com/magit/magit/issues/1275
(defadvice magit-start-git (around lang-en_US activate)
  "Set LANG to en_US."
  (let ((process-environment process-environment))
    (setenv "LANG" "en_US")
    ad-do-it))

(provide 'init-local-git)

;;; init-local-git.el ends here
