;;; kyt-org.el --- kyt org parsing lib

;;; Commentary:
;;

;;; Code:

(require 'org-element)

(defun org-element-properties (properties element)
  "Extract the values from the PROPERTIES of an ELEMENT."
  (mapcar (lambda (property)
            (org-element-property property element))
          properties))

(define-derived-mode org-element-output-mode emacs-lisp-mode
  "org-element" "Displaying org-element parsed tree."
  (flycheck-mode -1)
  (read-only-mode)
  (toggle-truncate-lines 1))
(define-key org-element-output-mode-map (kbd "q") 'quit-window)

(defmacro with-output-to-org-element-buffer (bufname &rest body)
  "`with-output-to-temp-buffer', tuned for org-element output.
BUFNAME, BODY: same as `with-output-to-temp-buffer'."
  `(let ((temp-buffer-show-hook
          (append temp-buffer-show-hook (list 'org-element-output-mode))))
     (with-output-to-temp-buffer ,bufname ,@body)))

(defmacro print-value-to-org-element-buffer (bufname &rest body)
  "Print org-element parsed tree to a temp buffer.
BUFNAME, BODY: same as `with-output-to-temp-buffer'."
  `(with-output-to-org-element-buffer
    ,bufname
    (pp (progn ,@body))))

(defun kyt/org-element-print-at-point ()
  "Print `org-element-at-point'."
  (interactive)
  (print-value-to-org-element-buffer
   "*org-element-at-point*"
   (org-element-at-point)))

(defun kyt/org-element-parse (begin end)
  "Recursively parse region between BEGIN and END."
  (org-element--parse-elements
   begin end
   ;; args come from `org-element-parse-buffer'
   'first-section nil nil nil (list 'org-data nil)))

(defun kyt/org-element-print (begin end)
  "Recursively parse region between BEGIN and END and print it."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let ((element (org-element-at-point)))
                   (list (org-element-property :begin element)
                         (org-element-property :end element)))
                 ))
  (print-value-to-org-element-buffer
   "*org-element-parsed*"
   (kyt/org-element-parse begin end)))

(provide 'kyt-org)

;;; kyt-org.el ends here
