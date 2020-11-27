;;; init-local-r.el --- my settings for R

;;; Commentary:
;;

;;; Code:


(declare-function require-package "init-elpa")
(require-package 'ess)
(require-package 'ess-view)
;; remove this because it is not on melpa for now
;; (require-package 'ess-R-object-popup)
(require-package 'ess-smart-underscore)

(defvar inferior-ess-mode-map)
(add-hook 'inferior-ess-mode-hook
          (lambda ()
            (defvar ess-S-underscore-when-last-character-is-a-space)
            (setq ess-S-underscore-when-last-character-is-a-space t)
            (define-key inferior-ess-mode-map (kbd "C-c C-g") 'ess-R-object-popup)))


(provide 'init-local-r)

;;; init-local-r.el ends here
