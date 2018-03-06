;;; init-local-flycheck.el --- Summary -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(declare-function after-load "init-utils")

(after-load 'flycheck
  (defvar flycheck-emacs-lisp-load-path)
  (defvar kyt/init-dir)
  ;; (setq flycheck-emacs-lisp-load-path (list kyt/init-dir))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  )

(provide 'init-local-flycheck)

;;; init-local-flycheck.el ends here
