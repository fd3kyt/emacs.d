;;; init-local.el --- Kyt local settings after purcell init

;;; Commentary:
;; 单独运行这个buffer会失败, 而且这里也显示错误信息
;; 但从init.el启动时,会成功
;;

;;; Code:
;; ===== personal global vars =====
;; .emacs.d/personal 作为集中个人设置的地方(相对 purcell 的设置)
(defvar my-emacs-dir
  (expand-file-name "personal"
                    user-emacs-directory)
  "集中个人设置的地方(相对 purcell 的设置).")

(defvar my-package-dir
  (expand-file-name "packages"
                    my-emacs-dir)
  "放置个人写的类似包的东西.")

(defvar my-init-dir
  (expand-file-name "load" my-emacs-dir)
  "Path to local init dir.")
(add-to-list 'load-path my-init-dir)

;; ===== personal global vars end =====

;; theme
(color-theme-sanityinc-solarized-light)

;; .md,.markdown 默认用gfm-mode打开
(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\)\\'" . gfm-mode) auto-mode-alist))

;; org-mode
(require 'org)
(require 'init-local-org)

;; save and recover setting
(require 'init-local-autosave)

;; scheme
;; (require 'init-local-scheme)

;; verilog
;;(require 'init-local-verilog)


(require 'graphviz-dot-mode)

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(require 'init-local-autoinsert)
(require 'init-local-ycmd)


;; temp c
(defun my-flycheck-c-setup ()
  (setq flycheck-gcc-language-standard "c99")
  (setq flycheck-clang-language-standard "c99"))

(add-hook 'c-mode-hook #'my-flycheck-c-setup)


;; local tools
;;(add-to-list 'load-path "~/.emacs.d/local/python-django/")
;;(require 'python-django)

;; mysite project
;;(setq python-shell-interpreter "~/.virtualenvs/mysite/bin/python")

;; highlight-phrase bug
(add-to-list 'ido-ubiquitous-command-overrides
             '(disable exact "highlight-phrase"))
(add-to-list 'ido-ubiquitous-command-overrides
             '(disable exact "highlight-lines-matching-regexp"))
(add-to-list 'ido-ubiquitous-command-overrides
             '(disable exact "unhighlight-regexp"))
(add-to-list 'ido-ubiquitous-command-overrides
             '(disable exact "highlight-regexp"))

;; global-magit-wip-save-mode bug:not existing dir
(global-magit-wip-save-mode 0);;disable

;; (add-to-list 'default-frame-alist '(height . 25))
;; (add-to-list 'default-frame-alist '(width . 80))

;; 这行应该在最下面
(require 'init-local-font)

(provide 'init-local)
;;; init-local.el ends here