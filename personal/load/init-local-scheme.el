;;; init-local-scheme.el --- scheme
;;; Code:

;;Quack for racket

;;; Commentary:

;; scheme
(require-package 'scheme)



;; geiser
;;(require-package 'geiser)
;;(setq geiser-active-implementations "racket")
;;(setq geiser-mode-start-repl-p t)




;; quack

(require-package 'quack)


(add-hook 'scheme-mode-hook 'enable-paredit-mode)
;;(add-hook 'inferior-scheme-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook
          (lambda ()
            (push '("lambda" . ?λ) prettify-symbols-alist)))

(define-key scheme-mode-map
  (kbd "C-c \\")
  'new-lambda-exp)


(defun mit-scheme-abort ()
  (interactive)
  (execute-kbd-macro [?\( ?r ?e ?s ?t ?a ?r ?t ?  ?1 return]))


(defun mit-scheme-force-abort ()
  (interactive)
  (execute-kbd-macro [?\C-c ?\C-\\ ?^ ?X return]))

(defun new-lambda-exp ()
  (interactive)
  (execute-kbd-macro "(lambda("))


(defun sicp ()
  (defun mit-scheme-init ()
    (define-key inferior-scheme-mode-map
      (kbd "M-(") ;; inferior-scheme 中s-m-m之类的无效
      'paredit-wrap-sexp)

    ;; 括号:成对括号
    (define-key inferior-scheme-mode-map
      (kbd "(")
      'insert-parentheses)

    (define-key inferior-scheme-mode-map
      (kbd "C-c r") ;; 修饰键大写
      'mit-scheme-abort)

    (define-key inferior-scheme-mode-map
      (kbd "C-c R")
      'mit-scheme-force-abort)

    (define-key inferior-scheme-mode-map
      (kbd "C-x C-f")
      'find-file)

    ;;
    (push '("]=>" . ?») prettify-symbols-alist)

    (prettify-symbols-mode 1))

  (interactive) ;; 必须有这一条才会出现在m-x里
  (cd "~/Dev/sicp/")
  (run-scheme  "mit-scheme")
  (mit-scheme-init))

;; prettify-symbols-mode

(global-prettify-symbols-mode 1)







(provide 'init-local-scheme)

;;; init-local-scheme.el ends here
