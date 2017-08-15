;;; init-local-tramp.el --- Tramp settings.

;;; Commentary:
;;

;;; Code:

(after-load 'tramp
  (setq tramp-default-method "ssh")
  (setq tramp-verbose 2))

(setq password-cache-expiry (* 15 60))




(provide 'init-local-tramp)

;;; init-local-tramp.el ends here
