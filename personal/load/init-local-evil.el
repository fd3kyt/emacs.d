;;; init-local-evil.el --- Try evil.

;;; Commentary:
;;

;;; Code:
(declare-function 'require-package "init-elpa")

(require-package 'evil)
(require-package 'evil-leader)

(evil-mode t)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
 "b" 'switch-to-buffer
 "w" 'save-buffer)

(pp (substitute-command-keys "\\{evil-normal-state-map}"))

(require-package 'help-fns+)
(require 'help-fns+)

(current-active-maps)


(provide 'init-local-evil)

;;; init-local-evil.el ends here
