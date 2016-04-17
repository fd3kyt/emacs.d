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

;;(setq org-startup-indented t);indent
;; 还是在 customize 里改吧

(defun org-mode-startup-settings ()
  "Set up org mode after it start."
  (add-hook 'auto-save-hook 'org-save-all-org-buffers) ; autosave

  ;;(setq org-startup-indented t) ;indent
  (org-hide-starting-star)
  )



(setq org-directory "~/Documents")

;; org-capture
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; org-capture
;; 自动加载,避免变量未定义
(require 'org-capture)

;(global-set-key (kbd "C-c k") 'org-capture)

(defvar org-capture-local-template-dir "~/.emacs.d/templates/"
  "Path to 'org-capture' local templates dir.")
(setq org-capture-templates nil)

(setq org-capture-templates
      (append '(("d" "Django")
                ("ds" "tourial.org selection" entry
                 (file "~/Dev/tour1.7.org")
                 (file "~/.emacs.d/templates/org-capture-django-tourial.org")
                 :empty-lines 1)
                ("dc" "tourial.org kill ring" entry
                 (file "~/Dev/tour1.7.org")
                 (file "~/.emacs.d/templates/org-capture-django-tourial-killring.org")
                 :empty-lines 1 :unnarrowed)
                ("e" "emacs-lisp code" entry
                 (file "~/Documents/emacs_learn/emacs_lisp.org")
                 (file "~/.emacs.d/templates/org-capture-elisp.org")))
              org-capture-templates))

(define-key org-mode-map
  (kbd "C-M-<return>")
  'org-insert-heading-after-current)

;; macro for note
(define-key org-mode-map
  (kbd "C-c z") [?\C-c ?\C-z ?< ?q tab ?\C-y ?\M-q ?\M-> return return])

;; show code highlight
(setq org-src-fontify-natively t)

(add-to-list 'load-path "~/.emacs.d/local/kyt-org/")
(require 'org-screenshot)



;; ;; for LaTex
;; ;;(require 'org-latex)
;; (setq org-export-latex-listings t)
;; (add-to-list 'org-latex-classes
;;              '("org-article"
;;                "\\documentclass{org-article}
;;                  [NO-DEFAULT-PACKAGES]
;;                  [EXTRA]"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(provide 'init-local-org)

;;; init-local-org.el ends here
