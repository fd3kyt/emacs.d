;;; init-local.el --- Kyt local settings after purcell init

;;; Commentary:
;; 单独运行这个buffer会失败, 而且这里也显示错误信息
;; 但从init.el启动时,会成功
;;

;;; Code:

;; ATTENTION: don't put any code here, before setting the vars.

;; ===== personal global vars =====
(defconst *is-a-cygwin* (eq system-type 'darwin))


(defvar kyt/debug-var)
(set-variable 'kyt/debug-var "start")

;; .emacs.d/personal 作为集中个人设置的地方(相对 purcell 的设置)
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
;; ===== personal global vars end =====




;; ========== init start ==========

(require 'kyt-lib)

(when *is-a-cygwin*
  (require 'init-local-cygwin))

(setq desktop-save t)

;; install packages
(require-package 'realgud)

;; theme
(color-theme-sanityinc-solarized-light)

;; 默认不显示 menu
(menu-bar-mode 0)

;; global ispell dictionary
(ispell-change-dictionary "american" t)

;; org-mode
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
(set-variable 'kyt/debug-var "before font")
(add-hook 'after-make-window-system-frame-hooks
          (lambda ()
            (require 'init-local-font)))
(set-variable 'kyt/debug-var "after font")

;; fix sudo blocking
(setq projectile-mode-line " Projectile")

(setq dired-listing-switches "-aBhl  --group-directories-first")

(after-load 'cc-mode
  (require 'init-local-cpp)
  (require 'init-local-ycmd)
  (require 'init-local-rtags)
  ;; (if (and (-contains-p 'company-rtags company-backends)
  ;;          (-contains-p 'company-ycmd company-backends))
  ;;     (let ((new-group (list 'company-rtags 'company-ycmd)))
  ;;       (setq company-backends
  ;;             (cons new-group
  ;;                   (-remove (lambda (element)
  ;;                              (-contains-p new-group element))
  ;;                            company-backends))))
  ;;   )
  )

(require 'init-local-python)

(require 'init-local-fcitx)


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


(require-package 'ace-pinyin)
(ace-pinyin-global-mode)

(require 'init-local-image)
(require 'init-local-paredit)
(require 'init-local-autoinsert)
(require 'init-local-simple-key-bindings)
(require 'init-local-simple-util)
(require 'init-local-file-type)


(add-hook 'sh-mode-hook
          '(lambda ()
             (make-local-variable 'company-backends)
             (push 'company-shell company-backends)
             (push 'company-files company-backends)))


(setq delete-by-moving-to-trash t
      trash-directory "/home/Storage/.trash")

(global-auto-revert-mode t)             ; won't revert modified buffers


;; ;; dired
;; (add-hook 'dired-mode-hook 'dired-omit-mode)
;; (setq dired-omit-verbose nil)
;; (setq dired-omit-extensions (list ))
;; (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$")

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
  (setq projectile-enable-caching t))

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


(require 'init-local-goldfish)

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


;;It seems that `with-eval-after-load' doesn't work here.
(require 'ag)
(setq ag-arguments (append ag-arguments '("-U" "--ignore" ".git/")))

;; use M-u C-v instead
;; (global-set-key (kbd "C-S-j") 'scroll-up-line)
;; (global-set-key (kbd "C-S-k") 'scroll-down-line)
(setq next-screen-context-lines 3)

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


(yas-reload-all)

(provide 'init-local)
;;; init-local.el ends here
