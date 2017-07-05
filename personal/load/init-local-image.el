;;; init-local-image.el --- Everything about image.

;;; Commentary:
;;


;;; Code:


(defvar kyt/image-name-regexps (list ".png$" ".svg$"))
(setq-default revert-without-query kyt/image-name-regexps)

;; show as image at open
(custom-set-variables '(auto-image-file-mode t))

;; c-c to toggle image display
(add-hook 'nxml-mode-hook 'image-minor-mode)



(provide 'init-local-image)

;;; init-local-image.el ends here
