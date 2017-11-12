;;; kyt-org.el --- kyt org parsing lib

;;; Commentary:
;;

;;; Code:

(require 'org-element)
(require 'subr-x)


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

(defun kyt/org-element-parse-at-point ()
  "Parse at point.  Mainly for the argument of `interactive'."
  (apply 'kyt/org-element-parse
         (kyt/org-element-range-at-point)))

(defun kyt/org-element-print (tree)
  "Print TREE."
  (interactive (list (kyt/org-element-parse-at-point)))
  (print-value-to-org-element-buffer "*org-element-parsed*" tree))


;; ########## get dicts
;; (org-element--get-node-properties)  ;; use on headline

(defun org-element-node-string (node type)
  "Extract a string (or nil) from NODE, based on TYPE."
  (cond
   ((eq type 'plain-text)
    (substring-no-properties node))
   ((eq type 'headline)
    (substring-no-properties
     (car (org-element-property :title node))))
   ((eq type 'item)
    (let ((tag (car (org-element-property :tag node))))
      (when tag
        (substring-no-properties tag))))
   ((-contains-p '(keyword node-property) type)
    (s-join ": "
            (org-element-properties (list :key :value)
                                    node)))))

(defun org-element-extract-info (node)
  "Extract the info of NODE."
  (let ((type (org-element-type node)))
    (let ((extra-string (org-element-node-string node type)))
      (if extra-string
          (list type extra-string)
        (list type)))))

(defun org-element-tree-skeleton (tree)
  "Skeleton of TREE."
  (append (org-element-extract-info tree)
          (mapcar 'org-element-tree-skeleton
                  (org-element-contents tree))))

(defun kyt/org-element-print-skeleton (tree)
  "Print the skeletopn of TREE."
  (interactive (list (kyt/org-element-parse-at-point)))
  (print-value-to-org-element-buffer
   "*org-element-skeleton*"
   (org-element-tree-skeleton tree)))

(defun koe-headline-section (headline)
  "Get the top level section of HEADLINE.

   Example usage:   (koe-headline-section (org-element-map
                          (kyt/org-element-parse-at-point)
                          'headline 'identity nil t))

Note: the output of `kyt/org-element-parse' is wrapped in
'org-data"
  (cl-assert (eq 'headline (org-element-type headline)))
  (let ((first-child (car (org-element-contents headline))))
    (when (and first-child
               (eq 'section (org-element-type first-child)))
      first-child)))


(provide 'kyt-org)

;;; kyt-org.el ends here
