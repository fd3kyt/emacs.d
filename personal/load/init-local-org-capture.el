;;; init-local-org-capture.el --- Initialize org-capture

;;; Commentary:
;;

;; Code
;; org-capture
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; org-capture
;; 自动加载,避免变量未定义
(require 'org-capture)

;; (global-set-key (kbd "C-c k") 'org-capture)

(defvar my-org-capture-template-dir
  (expand-file-name "templates" kyt/personal-dir)
  "Path to 'org-capture' local templates dir.")
(setq org-capture-templates nil)

(setq org-capture-templates
      (append '(("d" "Django")
                ("ds" "tourial.org selection" entry
                 (file "~/Dev/tour1.7.org")
                 (file "~/.emacs.d/personal/templates/org-capture-django-tourial.org")
                 :empty-lines 1)
                ("dc" "tourial.org kill ring" entry
                 (file "~/Dev/tour1.7.org")
                 (file "~/.emacs.d/personal/templates/org-capture-django-tourial-killring.org")
                 :empty-lines 1 :unnarrowed)
                ("e" "emacs-lisp code" entry
                 (file "~/Documents/emacs_learn/emacs_lisp.org")
                 (file "~/.emacs.d/personal/templates/org-capture-elisp.org"))
                ("n" "note")
                ("nn" "plain text note" entry
                 (file "~/Documents/notes.org")
                 (file "~/.emacs.d/personal/templates/org-capture-note.org"))
                ("np" "note of python" entry
                 (file+olp "~/Documents/python.org" "NOTES")
                 (file "~/.emacs.d/personal/templates/org-capture-note.org"))
                ("l" "time log" entry
                 (file+datetree "~/Documents/time_log.org")
                 (file "~/.emacs.d/personal/templates/org-capture-note.org")
                 :clock-in t :clock-keep t
                 ))
              org-capture-templates))


(provide 'init-local-org-capture)

;;; init-local-org-capture.el ends here
