;;; init-local-r.el --- my settings for R

;;; Commentary:
;;

;;; Code:


(require-package 'ess)
(require-package 'ess-view)
(require-package 'ess-R-object-popup)
(require-package 'ess-smart-underscore)

(add-hook 'inferior-ess-mode-hook
          (lambda ()
            (require 'ess-view)
            (require 'ess-R-object-popup)
            (require 'ess-smart-underscore)
            (custom-set-variables `(ess-S-underscore-when-last-character-is-a-space t))
            (define-key inferior-ess-mode-map (kbd "C-c C-g") 'ess-R-object-popup)))


(provide 'init-local-r)

;;; init-local-r.el ends here
