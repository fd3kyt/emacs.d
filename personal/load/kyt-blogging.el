;;; kyt-blogging.el --- Summary -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(declare-function require-package "init-elpa")

(require 'init-local-koe)
(require 'koe-util)

(defun kyt/org-link-at-point ()
  "Return the link string at point."
  (file-truename (org-element-property :path (koe-first-of-types 'link))))

(defun kyt/copy-org-link-at-point ()
  "Copy."
  (interactive)
  (kill-new (kyt/org-link-at-point)))

(require-package 'git-auto-commit-mode)
(require 'git-auto-commit-mode)


(provide 'kyt-blogging)

;;; kyt-blogging.el ends here
