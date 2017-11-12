;;; koe-tree-mode.el --- Mode and macro for displaying parse tree.

;;; Commentary:
;;

;;; Code:

(require 'org-element)

(define-derived-mode koe-tree-mode emacs-lisp-mode
  "org-element" "Displaying org-element parsed tree."
  (flycheck-mode -1)
  (read-only-mode)
  (toggle-truncate-lines 1))
(define-key koe-tree-mode-map (kbd "q") 'quit-window)

;; TODO outline the element/object types
;; temp :: (highlight-phrase "([A-Za-z][A-Za-z-0-9]+" (quote hi-yellow))

(defmacro with-koe-output-to-temp-buffer (bufname &rest body)
  "`with-output-to-temp-buffer', tuned for org-element output.
BUFNAME, BODY: same as `with-output-to-temp-buffer'."
  `(let ((temp-buffer-show-hook
          (append temp-buffer-show-hook (list 'koe-tree-mode))))
     (with-output-to-temp-buffer ,bufname ,@body)))

(defmacro print-parse-tree-to-temp-buffer (bufname &rest body)
  "Print org-element parsed tree to a temp buffer.
BUFNAME, BODY: same as `with-output-to-temp-buffer'."
  `(with-koe-output-to-temp-buffer
    ,bufname
    (pp (progn ,@body))))

;; TODO print to temp buffer directly and don't need
;; `with-koe-output-to-temp-buffer'?

(defun koe-print-at-point ()
  "Print `org-element-at-point'."
  (interactive)
  (print-parse-tree-to-temp-buffer
   "*org-element-at-point*"
   (org-element-at-point)))

(provide 'koe-tree-mode)

;;; koe-tree-mode.el ends here
