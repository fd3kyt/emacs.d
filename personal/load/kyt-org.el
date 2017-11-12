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

;; TODO outline the element/object types
;; temp :: (highlight-phrase "([A-Za-z][A-Za-z-0-9]+" (quote hi-yellow))

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

(defun kyt/org-element-range-at-point ()
  "Return the beginning point and the end point."
  (cond
   ((eq (point) (point-min))
    (list (point-min) (point-max)))
   ((use-region-p)
    (list (region-beginning) (region-end)))
   (t
    (let ((element (org-element-at-point)))
      (list (org-element-property :begin element)
            (org-element-property :end element))))))

(defun kyt/org-element-print (begin end)
  "Recursively parse region between BEGIN and END and print it."
  (interactive (kyt/org-element-range-at-point))
  (print-value-to-org-element-buffer
   "*org-element-parsed*"
   (kyt/org-element-parse begin end)))


;; ########## get dicts
;; (org-element--get-node-properties)  ;; use on headline

(defun org-element-extract-info (node)
  "Extract the info of NODE."
  (let ((type (org-element-type node)))
    (cond
     ((eq type 'plain-text)
      (list type (substring-no-properties node)))
     ((eq type 'headline)
      (list type
            (substring-no-properties
             (car (org-element-property :title node)))))
     ((eq type 'item)
      (list type
            (let ((tag (car (org-element-property :tag node))))
              (when tag
                (substring-no-properties tag)))))
     (t (list type)))))

(defun org-element-tree-skeleton (tree)
  "Skeleton of TREE."
  (append (org-element-extract-info tree)
          (mapcar 'org-element-tree-skeleton
                  (org-element-contents tree))))

(defun kyt/org-element-print-skeleton (begin end)
  "Print the skeletopn of parsed tree between BEGIN and END."
  (interactive (kyt/org-element-range-at-point))
  (print-value-to-org-element-buffer
   "*org-element-skeleton*"
   (org-element-tree-skeleton
    (kyt/org-element-parse begin end))))


"
(org-data
 (section
  (keyword))
 (headline
  (section
   (property-drawer)))
 )
"


;;


(provide 'kyt-org)

;;; kyt-org.el ends here
