;;; init-local-ox-hugo.el --- Summary -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:


(declare-function require-package "init-elpa")
(require-package 'ox-hugo)

(defvar org-hugo-default-section-directory)

(with-eval-after-load 'ox
  (require 'ox-hugo)
  (setq org-hugo-default-section-directory "posts"))

(provide 'init-local-ox-hugo)

;;; init-local-ox-hugo.el ends here
