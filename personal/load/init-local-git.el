;;; init-local-git.el --- Initialize git, magit for me.

;;; Commentary:
;;

;;; Code:

(after-load 'magit
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'init-local-git)

;;; init-local-git.el ends here
