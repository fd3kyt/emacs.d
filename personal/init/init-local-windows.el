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

;;; Fix: emacs + sogou input + MyBigCtrlForEmacs.ahk: when ahk send
;;; space, will sometimes insert two spaces instead of one. See
;;; MyBigCtrlForEmacs.ahk for details.
(define-key key-translation-map (kbd "<f20>") (kbd "SPC"))
(define-key key-translation-map (kbd "M-<f20>") (kbd "M-SPC"))
(define-key key-translation-map (kbd "C-<f20>") (kbd "C-SPC"))
(define-key key-translation-map (kbd "M-C-<f20>") (kbd "M-C-SPC"))

;; ;;; this works but too slow
;; (require 'init-local-fcitx)
;; (customize-set-variable
;;  'fcitx/im-turn-on-command
;;  "d:/nutstore/diy/Windows/ime-switch/switch-language.exe 00000804")
;; (customize-set-variable
;;  'fcitx/im-turn-off-command
;;  "d:/nutstore/diy/Windows/ime-switch/switch-language.exe 00000409")
;; (customize-set-variable
;;  'fcitx/im-test-if-turned-on-command
;;  "d:/nutstore/diy/Windows/ime-switch/switch-language.exe =00000804")

;;; fix windows shell command garbled:
;; beta way: Windows region setting: use utf-8 for global support
(defun fix-shell-command-garbled-in-windows (fun &rest args)
  "Call FUN with ARGS."
  (let ((coding-system-for-read 'gbk)
        (coding-system-for-write 'gbk))
    (apply fun args)))
(advice-add 'shell-command :around 'fix-shell-command-garbled-in-windows)
(defun fix-shell-mode-garbled-in-windows ()
  "Fix shell garbled in Windows."
  (set-process-coding-system (get-buffer-process (current-buffer)) 'gbk 'gbk))
(add-hook 'shell-mode-hook 'fix-shell-mode-garbled-in-windows)

;;; use powershell as default shell
;; (setq shell-file-name "powershell")
;; XXX will break M-x shell, use eshell instead; will break ag

(provide 'init-local-windows)

;;; init-local-windows.el ends here
