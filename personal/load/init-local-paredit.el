;;; init-local-paredit.el --- Extra initialization for paredit

;;; Commentary:
;;

;;; Code:
(after-load 'org
  (require-package 'paredit)
  (defvar org-mode-map)
  (define-paredit-pair ?\= ?\= "equal")
  (define-paredit-pair ?\~ ?\~ "tilde")
  (define-paredit-pair ?\* ?\* "star")
  (define-paredit-pair ?\+ ?\+ "plus")
  (define-key org-mode-map (kbd "M-*") 'paredit-wrap-star)
  (define-key org-mode-map (kbd "M-~") 'paredit-wrap-tilde)
  (define-key org-mode-map (kbd "M-+") 'paredit-wrap-equal)
  )


(provide 'init-local-paredit)

;;; init-local-paredit.el ends here
