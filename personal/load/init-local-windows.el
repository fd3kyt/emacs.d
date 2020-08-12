;;; init-local-windows.el --- Summary -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(setq w32-recognize-altgr nil)

;;; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
;;; doesn't work?
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; use a ls ported to Windows
;; (setq ls-lisp-use-insert-directory-program t)
;; (setq insert-directory-program "a-ls-ported-to-windows")

(setq ls-lisp-dirs-first t)
(setq dired-listing-switches "-al")

(provide 'init-local-windows)

;;; init-local-windows.el ends here
