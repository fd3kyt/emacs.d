;;; init-local-flycheck.el --- Summary -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(declare-function after-load "init-utils")

(after-load 'flycheck
  (defvar flycheck-emacs-lisp-load-path)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (defvar flycheck-checker-error-threshold)
  (setq flycheck-checker-error-threshold 200)
  )

(provide 'init-local-flycheck)

;;; init-local-flycheck.el ends here
