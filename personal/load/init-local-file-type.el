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

(provide 'init-local-file-type)

;;; init-local-file-type.el ends here
