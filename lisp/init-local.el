;;; init-local.el --- Kyt local settings after purcell init

;;; Commentary:
;; 单独运行这个buffer会失败, 而且这里也显示错误信息
;; 但从init.el启动时,会成功
;;

;;; Code:
;; ===== personal global vars =====
;; .emacs.d/personal 作为集中个人设置的地方(相对 purcell 的设置)

(defvar kyt/debug-var)
(set-variable 'kyt/debug-var "start")

(defvar kyt/personal-dir
  (expand-file-name "personal"
                    user-emacs-directory)
  "集中个人设置的地方(相对 purcell 的设置).")

(defvar kyt/package-dir
  (expand-file-name "packages"
                    kyt/personal-dir)
  "放置个人写的类似包的东西.")

(defvar kyt/init-dir
  (expand-file-name "load" kyt/personal-dir)
  "Path to local init dir.")
(add-to-list 'load-path kyt/init-dir)

;; ===== if under Windows =====
(when (equal window-system 'w32)
  (require 'init-local-w32))

;; ===== personal global vars end =====

;; theme
(color-theme-sanityinc-solarized-light)

;; 默认不显示 menu
(menu-bar-mode 0)

;; global ispell dictionary
(ispell-change-dictionary "american" t)

;; .md,.markdown 默认用gfm-mode打开
(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\)\\'" . gfm-mode) auto-mode-alist))

;; org-mode
(after-load 'org (require 'init-local-org))

;; save and recover setting
(require 'init-local-autosave)

;; scheme
;; (require 'init-local-scheme)

;; verilog
;;(require 'init-local-verilog)


(require 'graphviz-dot-mode)

;;(require 'init-local-autoinsert)

;; temp c
(defun my-flycheck-c-setup ()
  "Set flycheck standard flag."
  (setq flycheck-gcc-language-standard "c99")
  (setq flycheck-clang-language-standard "c99"))

(add-hook 'c-mode-hook #'my-flycheck-c-setup)


;; local tools
;;(add-to-list 'load-path "~/.emacs.d/local/python-django/")
;;(require 'python-django)

;; mysite project
;;(setq python-shell-interpreter "~/.virtualenvs/mysite/bin/python")

;; ;; highlight-phrase bug
;; (add-to-list 'ido-ubiquitous-command-overrides
;;              '(disable exact "highlight-phrase"))
;; (add-to-list 'ido-ubiquitous-command-overrides
;;              '(disable exact "highlight-lines-matching-regexp"))
;; (add-to-list 'ido-ubiquitous-command-overrides
;;              '(disable exact "unhighlight-regexp"))
;; (add-to-list 'ido-ubiquitous-command-overrides
;;              '(disable exact "highlight-regexp"))

;; ;; global-magit-wip-save-mode bug:not existing dir
;; (global-magit-wip-save-mode 0);;disable

;; (add-to-list 'default-frame-alist '(height . 25))
;; (add-to-list 'default-frame-alist '(width . 80))

(set-variable 'kyt/debug-var "before font")

;; ;; https://github.com/tuhdo/semantic-stickyfunc-enhance
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; (semantic-mode 1)
;; (require 'stickyfunc-enhance)

;; 解决 daemon 中设置 font 的问题
(if (daemonp)
    (add-hook 'after-make-window-system-frame-hooks
              (lambda ()
                (require 'init-local-font)))
  (require 'init-local-font))

(set-variable 'kyt/debug-var "after font")

;; fix sudo blocking
(setq projectile-mode-line " Projectile")

(setq dired-listing-switches "-aBhl  --group-directories-first")

(after-load 'cc-mode
  (require 'init-local-cpp)
  (require 'init-local-ycmd))

(require 'init-local-python)

;; (require 'init-local-fcitx)
;; 使用我的 fcitx3
(add-to-list 'load-path
             (expand-file-name "fcitx"
                               kyt/package-dir))
(require 'fcitx3)
(fcitx/enable)

(defun load-frequently-used ()
  "Load packges that I use frequently."
  (interactive)
  (require 'magit)
  (require 'org)
  (require 'python))

(require 'init-local-git)

;; (after-load 'company-ispell
;;   (global-set-key (kbd "C-c i")
;;                   'company-ispell))
(require 'init-local-hydra)

(global-set-key (kbd "C-c C-<")
                'mc/mark-all-like-this-dwim)

(require-package 'yasnippet)
(yas-global-mode)
(require 'init-local-snippet)

;; shows errors under point in pos-tip popups
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(global-set-key (kbd "C-S-t") 'transpose-words)
(global-set-key (kbd "M-t") 'transpose-sexps)

;; ftw
(setenv "http_proxy" "http://localhost:8123")
(setenv "https_proxy" "http://localhost:8123")

;; octave
(setq auto-mode-alist
      (append '(("\\.m\\'" . octave-mode))
              auto-mode-alist))


(provide 'init-local)
;;; init-local.el ends here