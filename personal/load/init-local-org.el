;;; init-local-org.el --- initialize org

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

(defvar org-startup-indented)
(setq org-startup-indented t)              ;indent

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
  (org-hide-starting-star))
(add-hook 'org-mode-hook 'org-mode-startup-settings)

(defvar org-directory)
(setq org-directory "~/Documents")

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
(setq org-src-fontify-natively t)

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

(setq org-ellipsis "↓")

;; useful tweak

(setq org-tag-persistent-alist
      '(("workflow" . ?w) ("log" . ?l) ("question" . ?q) ("summary" . ?s)) )

(setq org-hide-emphasis-markers nil)

(defun org-refile-goto ()
  "Go to heading using `org-refile'."
  (interactive)
  (org-refile '(4)))
(define-key org-mode-map (kbd "C-\\") 'org-refile-goto)


;; set the faces

(setq org-fontify-quote-and-verse-blocks t)

(setq dark-color "DimGray")

(set-face-attribute 'org-block-begin-line nil
                    :weight 'bold
                    :underline nil
                    :background "#F0F0F0")

(set-face-attribute 'org-quote nil
                    :background "#F5F2EC"
                    :weight 'normal
                    :foreground dark-color
                    :inherit 'org-block-begin-line)

(set-face-attribute 'org-block-end-line nil
                    :strike-through "grey"
                    :foreground "grey"
                    :weight 'bold
                    :inherit 'org-quote)

(set-face-attribute 'org-block nil
                    :foreground dark-color
                    :inherit 'org-quote)

(set-face-attribute 'org-verse nil
                    :inherit 'org-quote)

(set-face-attribute 'org-code nil
                    :inherit 'org-block-begin-line)

(set-face-attribute 'org-verbatim nil
                    :inherit 'org-code)

(set-face-attribute 'org-meta-line nil
                    :underline dark-color)

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

(setq org-plantuml-jar-path "~/local/plantuml.jar")

;; try org-brain
(require-package 'org-brain)
(require 'org-brain)
(setq org-id-track-globally t)
(setq org-brain-path "~/Documents/")

(provide 'init-local-org)

;;; init-local-org.el ends here
