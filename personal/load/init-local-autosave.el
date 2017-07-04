;;; init-local-autosave.el --- my setting on auto save

;;; Commentary:
;;

(custom-set-variables `(auto-save-interval 300)
                      `(auto-save-timeout 120))

;; By default
;; (desktop-save-mode 1)
(custom-set-variables `(desktop-restore-eager 0))

;;; Code:

(provide 'init-local-autosave)

;;; init-local-autosave.el ends here
