;;; init-local-ycmd.el --- initialize ycmd (for cpp)

;;; Commentary:
;; https://github.com/abingham/emacs-ycmd

;;; Code:
(declare-function require-package "init-elpa")
(require-package 'ycmd)

;; personal setting
(defvar kyt/ycmd-path (expand-file-name "~/local/ycmd/")
  "Ycmd projcet path.")

;;;;;;;;;;;;;;;;;;;emacs-ycmd;;;;;;;;;;;;;;;;;;;
;; (add-hook 'after-init-hook #'global-ycmd-mode)
(add-hook 'c-mode-common-hook #'ycmd-mode)
(customize-set-variable 'ycmd-force-semantic-completion t)
;;(customize-set-variable 'ycmd-global-config nil)
(customize-set-variable 'ycmd-server-command
                        (list "python" (expand-file-name "ycmd/"
                                                         kyt/ycmd-path)))
(customize-set-variable 'ycmd-global-config
                        (expand-file-name "cpp/ycm/.ycm_extra_conf.py"
                                          kyt/ycmd-path))
(customize-set-variable 'ycmd-extra-conf-whitelist
                        (list (expand-file-name "~/Projects/*")))
(customize-set-variable 'ycmd-extra-conf-handler
                        'load)

(defvar ycmd-startup-timeout)
(setq ycmd-startup-timeout 20)

(require-package 'company-ycmd)
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
