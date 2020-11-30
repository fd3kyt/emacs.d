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

;; use M-1, M-2 ... to switch window
;; https://github.com/deb0ch/emacs-winum
(require-package 'winum)
(defvar winum-keymap)
;;; need to do this BEFORE (require 'winum)
(setq winum-keymap
      (let ((map (make-sparse-keymap)))
        ;; (define-key map (kbd "C-`") 'winum-select-window-by-number)
        ;; (define-key map (kbd "C-²") 'winum-select-window-by-number)
        (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
        (define-key map (kbd "M-1") 'winum-select-window-1)
        (define-key map (kbd "M-2") 'winum-select-window-2)
        (define-key map (kbd "M-3") 'winum-select-window-3)
        (define-key map (kbd "M-4") 'winum-select-window-4)
        (define-key map (kbd "M-5") 'winum-select-window-5)
        (define-key map (kbd "M-6") 'winum-select-window-6)
        (define-key map (kbd "M-7") 'winum-select-window-7)
        (define-key map (kbd "M-8") 'winum-select-window-8)
        (define-key map (kbd "M-9") 'winum-select-window-9)
        map))
(defvar winum-scope)
(setq winum-scope 'frame-local)
(require 'winum)
(winum-mode)

;;; blink your cursor when you scroll.
(require-package 'beacon)
(require 'beacon)
(setq beacon-size 10
      beacon-color (face-attribute 'cursor :background)
      ;; beacon-push-mark 35
      beacon-blink-duration 0.1
      beacon-blink-delay 0.2
      )
(beacon-mode 1)


(provide 'init-local-simple-util)

;;; init-local-simple-util.el ends here
