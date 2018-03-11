;;; init-local-cpp.el --- my settings for cpp

;;; Commentary:
;;

;;; Code:

(require-package 'cc-mode)
(require-package 'flycheck-pos-tip)

;; c
(setq-default flycheck-gcc-language-standard "c99")
(setq-default flycheck-clang-language-standard "c99")

(require-package 'google-c-style)
;;(add-hook 'c-mode-common-hook 'google-set-c-style)
;;(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; (defvar c-basic-offset)
;; (setq c-basic-offset 4)

(defun kyt/set-c-style ()
  "Set up c style.  注意, \"add-hook\" 时, 是加在现有 hook 的前面.
容易搞混.  这里直接写一个完整的 style 设置"
  (google-set-c-style)
  (c-add-style "Google" '((c-basic-offset . 4)) t)
  (google-make-newline-indent))
(add-hook 'c-mode-common-hook #'kyt/set-c-style)

;; https://github.com/tuhdo/semantic-refactor
(require-package 'srefactor)
(require 'srefactor-lisp)

;; OPTIONAL: ADD IT ONLY IF YOU USE C/C++.
(semantic-mode 1) ;; -> this is optional for Lisp

(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
(global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
(global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
(global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer)


(define-key c-mode-map [remap ff-find-other-file]
  'projectile-find-other-file)

(define-key c++-mode-map [remap ff-find-other-file]
  'projectile-find-other-file)

;; c-c o switch between header and implementation
;; http://emacs-fu.blogspot.com/2008/12/quickly-switching-between-header-and.html
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(fset 'c-header-guard
      [?\C-e ?_ ?\C-. ?\C-a ?\M-x ?u ?p ?c ?a ?s ?e ?- ?r ?e ?g ?i ?o ?n return ?\C-. ?\C-e ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?s ?t ?r ?i ?n ?g return ?  return ?_ return ?\C-e ?\C-\M-b ?\C-\M-k ?# ?i ?f ?n ?d ?e ?f ?  ?\C-y return ?# ?d ?e ?f ?i ?n ?e ?  ?\C-y return return ?# ?e ?n ?d ?i ?f ?  ?/ ?/ ?  ?\C-y ?\C-p ?\C-o ?\C-o ?\C-n])


(require 'xref)
(require-package 'gxref)
(require 'gxref)
(add-to-list 'xref-backend-functions 'gxref-xref-backend)
;; only prompt for identifier when there is nothing at point or with
;; `universal-argument'.
(setq xref-prompt-for-identifier nil)


(after-load 'projectile
  (projectile-register-project-type
   'c++ '("CMakeLists.txt" "src" "build")
   :compile "mkdir -p build && cd build && cmake .. && make"
   :test "cd build && ctest"
   :test-prefix "test_"))

(defun kyt/gtags-current-project ()
  "Generate gtags tag files for current project."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-process "gtags")))

(defun kyt/add-to-rtags ()
  "Generate gtags tag files for current project."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-process "add-to-rtags.sh")))

;;; fix: some goldfish cpp files seem to disable undo by default
(add-hook 'c-mode-common-hook 'buffer-enable-undo)

(provide 'init-local-cpp)

;;; init-local-cpp.el ends here
