;;; init-local-file-type.el --- Set up things related to file type.

;;; Commentary:
;; Things like `auto-mode-alist'...

;;; Code:

(defvar kyt/image-name-regexps (list ".png$" ".svg$"))

(setq auto-mode-alist
      (append '(
                ("\\.\\(md\\|markdown\\)\\'" . gfm-mode)  ;markdown
                ("\\.m\\'" . octave-mode)                 ;octave
                )
              auto-mode-alist))

(custom-set-variables
 '(revert-without-query kyt/image-name-regexps))

(provide 'init-local-file-type)

;;; init-local-file-type.el ends here
