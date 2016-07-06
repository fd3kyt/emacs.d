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

(provide 'init-cpp-local)

;;; init-cpp-local.el ends here
