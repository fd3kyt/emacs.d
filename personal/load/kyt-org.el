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

(defun kyt/org-element-print-at-point ()
  "Print `org-element-at-point'."
  (interactive)
  (with-output-to-temp-buffer "*org-element-at-point*"
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
  (with-output-to-temp-buffer "*org-element-parsed*"
    (pp (kyt/org-element-parse begin end))))

(provide 'kyt-org)

;;; kyt-org.el ends here
