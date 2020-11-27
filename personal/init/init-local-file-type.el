;;; init-local-file-type.el --- Set up things related to file type.

;;; Commentary:
;; Things like `auto-mode-alist'...

;;; Code:

(setq auto-mode-alist
      (append '(
                ("\\.\\(md\\|markdown\\)\\'" . gfm-mode)  ;markdown
                ("\\.m\\'" . octave-mode)                 ;octave
                )
              auto-mode-alist))


(require-package 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

(setq auto-mode-alist
      (append '(("\\.h\\'" . c++-mode))
              auto-mode-alist))

(provide 'init-local-file-type)

;;; init-local-file-type.el ends here
