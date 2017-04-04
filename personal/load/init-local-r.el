;;; init-local-r.el --- my settings for R

;;; Commentary:
;;

;;; Code:


(require-package 'ess)
(require-package 'ess-view)
(require-package 'ess-R-object-popup)

(add-hook 'inferior-ess-mode-hook
          (lambda ()
            (require 'ess-view)
            (require 'ess-R-object-popup)
            (define-key inferior-ess-mode-map (kbd "C-c C-g") 'ess-R-object-popup)))


(provide 'init-local-r)

;;; init-local-r.el ends here
