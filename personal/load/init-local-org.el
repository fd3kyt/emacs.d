;;; init-local-org.el --- local org init for fd3kyt

;;; Commentary:
;;


;;; Code:
(require 'org)

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

;; macro for note
(define-key org-mode-map
  (kbd "C-c z") [?\C-c ?\C-z ?< ?q tab ?\C-y ?\M-q ?\M-> return return])

;; show code highlight
(custom-set-variables `(org-src-fontify-natively t))

;; init org-attach
(defun kyt/org-attach-init ()
  (when (buffer-file-name)
    (setq-local org-attach-directory
                (concat (file-name-directory (buffer-file-name))
                        (file-name-nondirectory (buffer-file-name))
                        ".d/"))))
(add-hook 'org-mode-hook 'kyt/org-attach-init)

(require 'org-download)
(defun kyt/org-screenshot (prefix)
  "Call org-download-screenshot with frame minimized.
PREFIX: if not nil, do not minimize."
  (interactive "P")
  (if prefix
      (org-download-screenshot)
    (progn
      (make-frame-invisible)
      (with-demoted-errors "Error: %S"
        (org-download-screenshot))
      (make-frame-visible))
    ))
(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map (kbd "C-c M-d")
                             'kyt/org-screenshot)))

(add-hook 'org-mode-hook
          (lambda ()
            (custom-set-variables `(org-download-image-dir
                                    (concat "./"
                                            (file-name-nondirectory (buffer-file-name))
                                            ".d")))
            (advice-add 'org-download--dir-2
                        :filter-return
                        #'kyt/get-reasonable-file-name)))

;; (add-to-list 'load-path
;;              (expand-file-name "kyt-org"
;;                                kyt/package-dir))
;; (require 'org-screenshot)
;; use org-download-screenshot instead

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (C . t)
   (shell . t)
   ))

(custom-set-variables `(org-ellipsis "↓"))

;; useful tweak


(provide 'init-local-org)

;;; init-local-org.el ends here
