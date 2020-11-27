;;; kyt-blogging.el --- Summary -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(declare-function require-package "init-elpa")

(require 'init-local-akoe)
(require 'akoe-util)

(defun kyt/org-link-at-point ()
  "Return the link string at point."
  (file-truename (org-element-property :path (akoe-first-of-types 'link))))

(defun kyt/copy-org-link-at-point ()
  "Copy."
  (interactive)
  (kill-new (kyt/org-link-at-point)))

(require-package 'git-auto-commit-mode)
;; (require 'git-auto-commit-mode) ; not found

(add-to-list 'safe-local-eval-forms
             '(add-hook 'after-save-hook #'org-hugo-export-wim-to-md-after-save :append :local))

(setq org-export-with-archived-trees nil)

(provide 'kyt-blogging)

;;; kyt-blogging.el ends here
