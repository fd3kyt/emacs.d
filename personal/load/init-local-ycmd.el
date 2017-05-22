;;; init-local-ycmd.el --- initialize ycmd (for cpp)

;;; Commentary:
;; https://github.com/abingham/emacs-ycmd

;;; Code:
;; (require 'ycmd)
;; (add-hook 'after-init-hook #'global-ycmd-mode)

;; (customize-set-variable 'ycmd-server-command '("python" "/home/fd3kyt/local/ycmd/ycmd/"))

;; (require 'company-ycmd)
;; (company-ycmd-setup)

;; (require 'flycheck-ycmd)
;; (flycheck-ycmd-setup)

;; (provide 'init-local-ycmd)

(require-package 'ycmd)
;;;;;;;;;;;;;;;;;;;emacs-ycmd;;;;;;;;;;;;;;;;;;;
;; (add-hook 'after-init-hook #'global-ycmd-mode)
(add-hook 'c-mode-common-hook #'ycmd-mode)
(customize-set-variable 'ycmd-force-semantic-completion t)
;;(customize-set-variable 'ycmd-global-config nil)
(customize-set-variable 'ycmd-server-command
                        '("python2" "/home/fd3kyt/local/ycmd/ycmd/"))
;;(ycmd-server-command (quote ("python" "/home/fd3kyt/local/ycmd/ycmd")))
(customize-set-variable 'ycmd-global-config
                        "/home/fd3kyt/local/ycmd/cpp/ycm/.ycm_extra_conf.py")
(customize-set-variable 'ycmd-extra-conf-whitelist
                        '("~/Projects/*"))
(customize-set-variable 'ycmd-extra-conf-handler
                        'load)

(require 'company-ycmd)
(company-ycmd-setup)
(global-company-mode)

(require-package 'flycheck-ycmd)
(flycheck-ycmd-setup)

(customize-set-variable 'company-clang-executable
                        "/usr/bin/clang-3.8")

;; (add-hook 'ycmd-file-parse-result-hook 'ycmd-display-file-parse-results)
;; (remove-hook 'ycmd-file-parse-result-hook 'ycmd-display-file-parse-results)

(provide 'init-local-ycmd)
;;; init-local-ycmd.el ends here
