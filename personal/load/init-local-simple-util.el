;;; init-local-simple-util.py --- Load small utility packages and set them up.

;;; Commentary:
;;

;;; Code:

(declare-function require-package "init-elpa")

(require-package 'bing-dict)
(global-set-key (kbd "C-c b") 'bing-dict-brief)

(require-package 'avy-zap)
(global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)
(global-set-key (kbd "M-Z") 'avy-zap-up-to-char-dwim)

;; ;; https://github.com/tuhdo/semantic-stickyfunc-enhance
;; (require-package 'stickyfunc-enhance)
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (semantic-mode 1)

;; see more in https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/guide-zh.org#持续改进
;; use M-1, M-2 ... to switch window
(require-package 'window-numbering)
;; (window-numbering-mode)

(provide 'init-local-simple-util)

;;; init-local-simple-util.el ends here
