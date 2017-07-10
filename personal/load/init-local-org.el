;;; init-local-org.el --- local org init for fd3kyt

;;; Commentary:
;;


;;; Code:
(require-package 'org)

(require-package 'org-download)

(defun org-hide-starting-star ()
  "Hide starting star when using org indent."
  (interactive)
  (set-face-background 'org-hide (face-background 'default));hide stars
  (set-face-foreground 'org-hide (face-background 'default)))

(custom-set-variables `(org-startup-indented t))              ;indent

(defun org-save-all-org-buffers-no-message ()
  "Call org-save-all-org-buffers, clear echo area if in minibuffer."
  (interactive)
  (org-save-all-org-buffers)
  (if (window-minibuffer-p)
      (message nil)))

(defun org-mode-startup-settings ()
  "Set up org mode after it start."
  (add-hook 'auto-save-hook
            'org-save-all-org-buffers-no-message) ; autosave

  ;;(custom-set-variables `(org-startup-indented t') ;indent
  (org-hide-starting-star))
(add-hook 'org-mode-hook 'org-mode-startup-settings)

(custom-set-variables `(org-directory "~/Documents"))

(require 'init-local-org-capture)

(define-key org-mode-map
  (kbd "C-M-<return>")
  'org-insert-heading-after-current)

(define-key org-mode-map
  (kbd "M-S-<return>")
  'org-insert-subheading)

;; macro for note
(define-key org-mode-map
  (kbd "C-c z") [?\C-c ?\C-z ?< ?q tab ?\C-y ?\M-q ?\M-> return return])

;; show code highlight
(custom-set-variables `(org-src-fontify-natively t))

(defun kyt/org-get-data-directory-name ()
  "Get the expected data directory name of current org file."
  (concat "./"
          (file-name-nondirectory
           (kyt/buffer-file-name-or-default))
          ".d"))

;; init org-attach
(defun kyt/org-attach-init ()
  "Initialization for org-attach."
  (setq-local org-attach-directory
              (kyt/org-get-data-directory-name)))
(add-hook 'org-mode-hook 'kyt/org-attach-init)

(require-package 'org-download)
(require 'org-download)
(defun kyt/org-screenshot (prefix)
  "Call org-download-screenshot with frame minimized.
PREFIX: if not nil, do not minimize."
  (interactive "P")
  (if prefix
      (org-download-screenshot)
    (progn
      (make-frame-invisible nil t)
      (with-demoted-errors "Error: %S"
        (org-download-screenshot))
      (make-frame-visible))
    ))

(after-load 'org
  (define-key org-mode-map (kbd "C-c M-d")
    'kyt/org-screenshot))


(add-hook 'org-mode-hook
          (lambda ()
            ;; same problem as setting kyt/org-attach-init. Maybe I
            ;; should create a function for this.
            (setq org-download-image-dir
                  (kyt/org-get-data-directory-name))
            (advice-add 'org-download--dir-2
                        :filter-return
                        #'kyt/get-reasonable-file-name)))

;; (add-to-list 'load-path
;;              (expand-file-name "kyt-org"
;;                                kyt/package-dir))
;; (require 'org-screenshot)
;; use org-download-screenshot instead

(custom-set-variables `(org-ellipsis "↓"))

;; useful tweak

(custom-set-variables
 `(org-tag-persistent-alist
   '(("workflow" . ?w) ("log" . ?l) ("question" . ?q) ("summary" . ?s)))
 `(org-hide-emphasis-markers nil))


(defun org-refile-goto ()
  "Go to heading using `org-refile'."
  (interactive)
  (org-refile '(4)))
(define-key org-mode-map (kbd "C-\\") 'org-refile-goto)


;; set the faces

(custom-set-variables '(org-fontify-quote-and-verse-blocks t))

(set-face-attribute 'org-block-begin-line nil
                    :weight 'bold
                    :underline nil
                    :background "#F0F0F0")

(set-face-attribute 'org-quote nil
                    :background "#F5F2EC"
                    :weight 'normal
                    :foreground "dark"
                    :inherit 'org-block-begin-line)

(set-face-attribute 'org-block-end-line nil
                    :strike-through "grey"
                    :foreground "grey"
                    :weight 'bold
                    :inherit 'org-quote)

(set-face-attribute 'org-block nil
                    :foreground "dark"
                    :inherit 'org-quote)

(set-face-attribute 'org-verse nil
                    :inherit 'org-quote)

(set-face-attribute 'org-code nil
                    :inherit 'org-block-begin-line)

(set-face-attribute 'org-verbatim nil
                    :inherit 'org-code)

(set-face-attribute 'org-meta-line nil
                    :underline "dark")

;;; Babel
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (C . t)
   (shell . t)
   (plantuml . t)
   ))

(custom-set-variables '(org-plantuml-jar-path "~/local/plantuml.jar"))

(provide 'init-local-org)

;;; init-local-org.el ends here
