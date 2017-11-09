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

(defmacro with-output-to-org-element-buffer (bufname &rest body)
  "`with-output-to-temp-buffer', tuned for org-element output.
BUFNAME, BODY: same as `with-output-to-temp-buffer'."
  `(let ((temp-buffer-show-hook
          '((lambda ()
              (emacs-lisp-mode)
              (flycheck-mode -1)
              (read-only-mode)
              (toggle-truncate-lines 1)
              (define-key )))))
     (with-output-to-temp-buffer ,bufname ,@body)))
;; TODO make a minor mode and add key bindings (q).
;; print result

(defun kyt/org-element-print-at-point ()
  "Print `org-element-at-point'."
  (interactive)
  (with-output-to-org-element-buffer
   "*org-element-at-point*"
   (pp (org-element-at-point))))

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
  (with-output-to-org-element-buffer
   "*org-element-parsed*"
   (pp (kyt/org-element-parse begin end))))

(provide 'kyt-org)

;;; kyt-org.el ends here
