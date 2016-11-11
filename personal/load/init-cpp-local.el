;;; init-cpp-local.el --- my settings for cpp

;;; Commentary:
;;

;;; Code:

(require 'cc-mode)

(require 'google-c-style)
;;(add-hook 'c-mode-common-hook 'google-set-c-style)
;;(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(defun kyt/set-c-style ()
  "Set up c style.  注意, \"add-hook\" 时, 是加在现有 hook 的前面.
容易搞混.  这里直接写一个完整的 style 设置"
  (google-set-c-style)
  (google-make-newline-indent)
  (setq c-basic-offset 4))

(add-hook 'c-mode-common-hook #'kyt/set-c-style)

(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;; https://github.com/tuhdo/semantic-refactor
(require 'srefactor)
(require 'srefactor-lisp)

;; OPTIONAL: ADD IT ONLY IF YOU USE C/C++.
(semantic-mode 1) ;; -> this is optional for Lisp

(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
(global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
(global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
(global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer)

;; c-c o switch between header and implementation
;; http://emacs-fu.blogspot.com/2008/12/quickly-switching-between-header-and.html
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(setq auto-mode-alist
      (append '(("\\.h\\'" . c++-mode))
              auto-mode-alist))

(fset 'c-header-guard
      [?\C-e ?_ ?\C-. ?\C-a ?\M-x ?u ?p ?c ?a ?s ?e ?- ?r ?e ?g ?i ?o ?n return ?\C-. ?\C-e ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?s ?t ?r ?i ?n ?g return ?  return ?_ return ?\C-e ?\C-\M-b ?\C-\M-k ?# ?i ?f ?n ?d ?e ?f ?  ?\C-y return ?# ?d ?e ?f ?i ?n ?e ?  ?\C-y return return ?# ?e ?n ?d ?i ?f ?  ?/ ?/ ?  ?\C-y ?\C-p ?\C-o ?\C-o ?\C-n])


(provide 'init-cpp-local)

;;; init-cpp-local.el ends here
