;;; init-local-org.el --- local org init for fd3kyt

;;; Commentary:
;;


;;; Code:
(require 'org)

(defun org-hide-starting-star ()
  "Hide starting star when using org indent."
  (interactive)
  (set-face-background 'org-hide (face-background 'default));hide stars
  (set-face-foreground 'org-hide (face-background 'default)))

(add-hook 'org-mode-hook 'org-mode-startup-settings)

(setq org-startup-indented t);indent

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

  ;;(setq org-startup-indented t) ;indent
  (org-hide-starting-star))

(setq org-directory "~/Documents")

;; org-capture
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; org-capture
;; 自动加载,避免变量未定义
(require 'org-capture)

;(global-set-key (kbd "C-c k") 'org-capture)

(defvar my-org-capture-template-dir
  (expand-file-name "templates" kyt/personal-dir)
  "Path to 'org-capture' local templates dir.")
(setq org-capture-templates nil)

;; (setq org-capture-templates
;;       (append '(("d" "Django")
;;                 ("ds" "tourial.org selection" entry
;;                  (file "~/Dev/tour1.7.org")
;;                  (file "~/.emacs.d/personal/templates/org-capture-django-tourial.org")
;;                  :empty-lines 1)
;;                 ("dc" "tourial.org kill ring" entry
;;                  (file "~/Dev/tour1.7.org")
;;                  (file "~/.emacs.d/personal/templates/org-capture-django-tourial-killring.org")
;;                  :empty-lines 1 :unnarrowed)
;;                 ("e" "emacs-lisp code" entry
;;                  (file "~/Documents/emacs_learn/emacs_lisp.org")
;;                  (file "~/.emacs.d/personal/templates/org-capture-elisp.org")))
;;               org-capture-templates))

(define-key org-mode-map
  (kbd "C-M-<return>")
  'org-insert-heading-after-current)

;; macro for note
(define-key org-mode-map
  (kbd "C-c z") [?\C-c ?\C-z ?< ?q tab ?\C-y ?\M-q ?\M-> return return])

;; show code highlight
(setq org-src-fontify-natively t)

;; init org-attach
(defun kyt/org-attach-init ()
  (setq-local org-attach-directory
              (concat (file-name-directory (buffer-file-name))
                      (file-name-nondirectory (buffer-file-name))
                      ".d/")))
(add-hook 'org-mode-hook 'kyt/org-attach-init)

(require 'org-download)
(defun kyt/org-screenshot (prefix)
  "Call org-download-screenshot with frame minimized.
PREFIX: if not nil, do not minmize."
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
                           (local-set-key (kbd "C-c M-d")
                                          'kyt/org-screenshot)))

;; (add-to-list 'load-path
;;              (expand-file-name "kyt-org"
;;                                kyt/package-dir))
;; (require 'org-screenshot)
;; use org-download-screenshot instead

(provide 'init-local-org)

;;; init-local-org.el ends here
