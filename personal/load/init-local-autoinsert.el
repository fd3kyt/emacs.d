;;; init-local-autoinsert.org --- templates for auto-insert

;;; Commentary:
;;


;;; Code:
(require 'autoinsert)
(autoload 'yas-expand-snippet "yasnippet")
(auto-insert-mode 1)  ;;; Adds hook to find-files-hook
(setq auto-insert-query nil) ;;; If you don't want to be prompted before insertion

(setq auto-insert-directory "~/.emacs.d/personal/auto-insert/")


(defun my-autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-minor-mode)
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(defun kyt/define-auto-insert (condition filename &rest functions)
  "Simple wrapper of `define-auto-insert'.
CONDITION and FILENAME is passed to `define-auto-insert.'
FUNCTIONS: extra functions to run after expanding the snippet."
  (princ functions)
  (define-auto-insert condition
    (vconcat (vector filename 'my-autoinsert-yas-expand)
             functions))
  )


;; workflow to add new auto-insert:
;;
;; add a file under `kyt/auto-insert-directory', the file contains the
;; *body* of a yasnippet
;;
;; add a (kyt/define-auto-insert <filename-pattern> <template_file>)
(defvar auto-insert-alist)
(setq auto-insert-alist nil)
(kyt/define-auto-insert "\\.org$" "auto-insert.org")
(kyt/define-auto-insert "\\.dot$" "auto-insert.dot")
(kyt/define-auto-insert "\\.sh$" "auto-insert.sh"
                        '(lambda () (sh-set-shell "bash")))


(provide 'init-local-autoinsert)
;;; init-local-autoinsert.el ends here
