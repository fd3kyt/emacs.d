;;; init-local-main.el --- Summary -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(declare-function require-package 'init-elpa)

(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

(require 'kyt-lib)

(defvar *is-a-cygwin*)
(when *is-a-cygwin*
  (require 'init-local-cygwin))

(defvar *is-a-windows*)
(when *is-a-windows*
  (require 'init-local-windows))

(setq source-directory "~/Sources.local/emacs/src")

;;; desktop
(defvar desktop-save)
(customize-set-variable 'desktop-save 'if-exists)
(customize-set-variable 'desktop-restore-eager 0)
(defvar desktop-base-file-name)
(setq desktop-base-file-name (format "~/.emacs.d/.emacs.%s.desktop"
                                     (kyt/get-system-name)))

;;; recentf
(defvar recentf-max-saved-items)
(defvar recentf-save-file)
(setq recentf-max-saved-items 200
      recentf-save-file (format "~/.emacs.d/%s.recentf"
                                (kyt/get-system-name)))

;; install packages
(require-package 'realgud)

;; theme
(color-theme-sanityinc-solarized-light)

;; 默认不显示 menu
(menu-bar-mode 0)

;;; ispell and english word completion:
;; should install "words" system package first. when installed, ispell
;; will find it when initializing `ispell-alternate-dictionary'. if
;; this doesn't work, check the help of `company-ispell-dictionary'

;; global ispell dictionary. may need to install dictionary
;; first. "sudo pacman -S hunspell-en_US"
(ispell-change-dictionary "american" t)

;; org-mode
(require-package 'org)
(require-package 'org-plus-contrib)
(after-load 'org (require 'init-local-org))

;; save and recover setting
(require 'init-local-autosave)

;; scheme
;; (require 'init-local-scheme)

;; verilog
;;(require 'init-local-verilog)


(require-package 'graphviz-dot-mode)
(require 'graphviz-dot-mode)

;; 解决 daemon 中设置 font 的问题
(add-hook 'after-make-window-system-frame-hooks
          (lambda ()
            (require 'init-local-font)))

(unless *is-a-windows*
  (setq dired-listing-switches "-aBhl  --group-directories-first"))

;; (after-load 'cc-mode
;;   (require 'init-local-cpp)
;;   (when *is-a-linux*
;;     (require 'init-local-ycmd)
;;     (require 'init-local-rtags)
;;     ;; make use of the completions from rtags and ycmd at the same time
;;     ;; (if (and (-contains-p company-backends 'company-rtags)
;;     ;;          (-contains-p company-backends 'company-ycmd))
;;     ;;     (let ((new-group (list 'company-rtags 'company-ycmd :separate)))
;;     ;;       (push new-group company-backends)))
;;     ;;; only use ycmd for completions. Fast, have fuzzy completions
;;     (setq company-backends (remove 'company-rtags company-backends))
;;     (setq rtags-completions-enabled nil)))

(require 'init-local-python)

(when *is-a-linux*
  (require 'init-local-fcitx))

(require 'init-local-git)
(require 'init-local-r)

;; (after-load 'company-ispell
;;   (global-set-key (kbd "C-c i")
;;                   'company-ispell))
(require 'init-local-hydra)

(require-package 'yasnippet)
(yas-global-mode)
(require 'init-local-snippet)

(require-package 'flycheck-pos-tip)
(require 'flycheck-pos-tip)

(with-eval-after-load "flycheck"
  ;; shows errors under point in pos-tip popups
  (flycheck-pos-tip-mode)
  (setq flycheck-idle-change-ydelay 0.5))

;; System locale to use for formatting time values.
(setq system-time-locale "C")

;; for R
(message "ATTENTION: setting $LANG to en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

;; ivy fuzzy
;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
;; (setq ivy-initial-inputs-alist nil)     ; no leading "^"
(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))

(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; disable semantic-mode to avoid hanging in comment
;; won't affect code completion dis
(add-hook 'emacs-lisp-mode-hook (lambda () (semantic-mode -1)))




;; temp
;; https://www.emacswiki.org/emacs/NxmlMode#toc11

(after-load 'nxml-mode
  (defun nxml-where ()
    "Display the hierarchy of XML elements the point is on as a path."
    (interactive)
    (let ((path nil))
      (save-excursion
        (save-restriction
          (widen)
          (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                      (condition-case nil
                          (progn
                            (nxml-backward-up-element) ; always returns nil
                            t)
                        (error nil)))
            (setq path (cons (xmltok-start-tag-local-name) path)))
          (if (called-interactively-p t)
              (progn (message "/%s" (mapconcat 'identity path "/"))
                     (kill-new (mapconcat 'identity path "/")))
            (format "/%s" (mapconcat 'identity path "/")))))))
  (defun nxml-where-header-line ()
    (interactive)
    (setq-local header-line-format (nxml-where)))
  (define-key nxml-mode-map (kbd "C-'") 'nxml-where)
  (defun setup-nxml-header-line ()
    (add-hook 'post-command-hook 'nxml-where-header-line t t))
  (add-hook 'nxml-mode-hook 'setup-nxml-header-line))


;; (require-package 'pinyinlib)
;; (require 'pinyinlib) ;used by 'ace-pinyin
;;;; not provided?
;; (require-package 'ace-pinyin)
;; (ace-pinyin-global-mode)

(require 'init-local-image)
(require 'init-local-paredit)
(require 'init-local-autoinsert)
(require 'init-local-simple-key-bindings)
(require 'init-local-simple-util)
(require 'init-local-file-type)

(require-package 'company-shell)

(add-hook 'sh-mode-hook
          '(lambda ()
             (make-local-variable 'company-backends)
             (push 'company-shell company-backends)
             ;; (push 'company-files company-backends)
             ))

(setq delete-by-moving-to-trash t
      trash-directory "/home/fd3kyt/.local/share/Trash/files/")

(global-auto-revert-mode t)             ; won't revert modified buffers


;;; dired
;; https://www.gnu.org/software/emacs/manual/html_node/dired-x/Installation.html#Installation
(add-hook 'dired-mode-hook
          (lambda ()
            (require 'dired-x)
            (dired-omit-mode 1)))
(setq dired-omit-verbose nil)
(setq dired-omit-extensions nil)
(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$")

;; try annotate
(require-package 'annotate)

(defun kyt/dired-du ()
  "Get the size of each marked files/directories."
  (interactive)
  (dired-do-shell-command "du -sh"
                          current-prefix-arg
                          (dired-get-marked-files t current-prefix-arg)))

(require 'init-local-tramp)

(defun kyt/ag-project-org (string regexp-p)
  "Ag STRING in ~/Projects/**/*.org.

If called with a prefix, use regexp (REGEXP-P will be t)."
  (interactive (let ((regexp-p (> (prefix-numeric-value current-prefix-arg) 1)))
                 (list (read-string (if regexp-p "Regexp: " "String: "))
                       regexp-p)))
  (let ((current-prefix-arg 1)  ;avoid passing down to `ag/search'
        (ag-arguments (append ag-arguments (list "-U"))))
    (ag/search string "~/Projects" :regexp regexp-p :file-regex "\\.org$"))
  )

(after-load 'projectile
  ;; fix sudo blocking
  (setq projectile-mode-line " Projectile")
  ;; only method `native' respects .projectile. `hybrid' respects
  ;; `projectile-globally-ignored-file-suffixes', ignores
  ;; .projectile. `alien' respects none.
  (customize-set-variable 'projectile-indexing-method 'hybrid)
  (customize-set-variable 'projectile-enable-caching t))

;; (require-package 'bookmark+)
;; (require 'bookmark+)
(setq bookmark-version-control t)
(setq bookmark-save-flag 1)             ; save on every modification
(setq bookmark-default-file (expand-file-name "bookmarks.el"
                                              kyt/personal-dir))

;; fix: aggressive-indent-mode causes bookmark+ to raise "Invalid
;; bookmark-file"
(defun without-aggressive-indent-mode (oldfun &rest rest)
  "Run OLDFUN using REST as arguments with `aggressive-indent-mode' turned off.

Read the first element in REST as the file name, open it, switch to
the buffer and turn off aggressive-indent-mode, then call `oldfun'.

Update: it seems that in order to make it work, we should keep
aggressive-indent-mode off instead of restoring its original
state."
  (let* ((file (car rest))
         (buffer-exist-p (get-file-buffer file)))
    (with-current-buffer (let ((enable-local-variables  ())) (find-file-noselect file))
      (aggressive-indent-mode -1)
      ;; fix existing bookmark file
      (save-excursion
        (goto-char (point-max))
        ;; don't need to replace if doesn't have spaces
        (when (re-search-backward "^\\s-+)" nil t)
          (replace-match ")")))
      (apply oldfun rest)
      (unless buffer-exist-p (kill-buffer (current-buffer))))
    )
  )
(advice-add 'bookmark-write-file :around 'without-aggressive-indent-mode)


;; (require 'init-local-goldfish)

;;; shellcheck: purcell use it. just install shellcheck and open a .sh
;;; file

(defun use-bash-when-zsh ()
  "Set shell to 'bash' if is a zsh file.  Why: shellcheck doesn't support zsh."
  (if (and buffer-file-name
           (string-match "\\.zsh$" buffer-file-name))
      (sh-set-shell "bash")))
(add-hook 'sh-mode-hook
          'use-bash-when-zsh)

;; image+
;; https://github.com/mhayashi1120/Emacs-imagex
(require-package 'image+)
(with-eval-after-load "image"
  (require 'image+)
  (imagex-global-sticky-mode 1)
  (imagex-auto-adjust-mode 1)
  (setq imagex-auto-adjust-threshold 1)
  (when (require 'hydra nil t)
    (defhydra imagex-sticky-binding (global-map "C-x C-l")
      "Manipulating Image"
      ("+" imagex-sticky-zoom-in "zoom in")
      ("-" imagex-sticky-zoom-out "zoom out")
      ("M" imagex-sticky-maximize "maximize")
      ("O" imagex-sticky-restore-original "restore original")
      ("S" imagex-sticky-save-image "save file")
      ("r" imagex-sticky-rotate-right "rotate right")
      ("l" imagex-sticky-rotate-left "rotate left"))))

;;; if I use ".#flycheck", pylint will complain about file naming
;;; style
(setq flycheck-temp-prefix "flychecktmp")

;;It seems that `with-eval-after-load' doesn't work here.
(require 'ag)
(with-eval-after-load "ag"
  (setq ag-arguments
        (append ag-arguments (list "-U" "--ignore" ".git/"
                                   "--ignore"
                                   (format "%s*" flycheck-temp-prefix)
                                   "--ignore" "^#"
                                   "--ignore" "^\\.#"
                                   "--width" "300"
                                   "-f"))))

;; use M-u C-v instead
;; (global-set-key (kbd "C-S-j") 'scroll-up-line)
;; (global-set-key (kbd "C-S-k") 'scroll-down-line)
(setq next-screen-context-lines 3)


;; #################### extra warning message ####################
;; (advice-add 'run-hooks :around 'kyt/warn-when-error)
;; (advice-remove 'run-hooks 'kyt/warn-when-error)

;; test this:

;; (defun my-throw-error ()
;;   "A testing error."
;;   (error "This is my-throw-error"))
;; (defvar my-hook nil)
;; (add-hook 'my-hook 'my-throw-error)
;; (advice-add 'run-hooks :around 'kyt/warn-when-error)
;; (run-hooks 'my-hook)




(setq flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)

(defun setup-which-key ()
  "Setup `which-key' during init."
  (when guide-key-mode
    (guide-key-mode -1))
  (require-package 'which-key)
  (require 'which-key)
  (which-key-setup-side-window-right)
  (which-key-mode 1))
;; append so that this happens after `guide-key-mode' from `init-editing-utils'
(add-hook 'after-init-hook 'setup-which-key t)

;; fix: `projectile-rails-global-mode-enable-in-buffers' in
;; `after-change-major-mode-hook' causes error.
(remove-hook 'after-change-major-mode-hook
             'projectile-rails-global-mode-enable-in-buffers)

(yas-reload-all)

(after-load 'flycheck (require 'init-local-flycheck))

(after-load 'org
  (require-package 'deferred)
  (require-package 'request-deferred)
  (require 'init-local-akoe)
  )

(global-set-key (kbd "C-x R") 'revert-buffer)
(global-set-key (kbd "<f8>") 'repeat-complex-command)

(setq uptimes-keep-count 50)
(setq uptimes-auto-save-interval (* 60 60)) ;a hour
(setq uptimes-database "~/.emacs.d/uptimes.uptimes")

(setq company-dabbrev-ignore-case nil
      company-dabbrev-downcase nil)
(setq company-dabbrev-code-everywhere t)

;; (require 'kyt-blogging)


;;; temp setting for graduation design
;; C-. doesn't work under sogou input
(global-set-key (kbd "C-'") 'set-mark-command)
(after-load 'org
  (define-key org-mode-map
    (kbd "C-'")
    'cua-set-mark))

(require 'init-local-graduation)

(global-set-key (kbd "C-c 3") 'kyt/copy-buffer-file-name)
(global-set-key (kbd "C-c C-3") 'kyt/copy-buffer-link)

(after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-c")
    'eval-buffer))

(after-load 'company
  (customize-set-variable 'company-show-numbers t))

;;; With `C-u', describe-function/variable to definition directly
(defun kyt/describe-function-jump-with-prefix (function)
  "With prefix arg, jump to FUNCTION and return non-nil."
  (when current-prefix-arg (find-function function) t))
(advice-add 'describe-function :before-until 'kyt/describe-function-jump-with-prefix)
(defun kyt/describe-variable-jump-with-prefix (variable)
  "With prefix arg, jump to VARIABLE and return non-nil."
  (when current-prefix-arg (find-variable variable) t))
(advice-add 'describe-variable :before-until 'kyt/describe-variable-jump-with-prefix)

(customize-set-variable 'fill-column 90)

(require-package 'ahk-mode)
(after-load 'ahk-mode
  (customize-set-variable 'ahk-indentation 4))

(require-package 'powershell)

;;; #################### post personal init: ####################

;;; init.el set `debug-on-error' to t to get backtraces during
;;; initialization.  After initialization, set it to nil for daily
;;; usage.
(add-hook 'after-init-hook (lambda () (setq debug-on-error nil)) 'append)
(provide 'init-local-main)

;;; init-local-main.el ends here
