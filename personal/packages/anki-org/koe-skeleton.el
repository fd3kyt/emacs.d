;;; koe-skeleton.el --- Print a simplify parse tree.

;;; Commentary:
;;

;;; Code:

(require 'koe-util)

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
            (koe-properties (list :key :value)
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

(defun koe-print-skeleton (tree)
  "Print the skeletopn of TREE."
  (interactive (list (koe-parse-at-point)))
  (print-parse-tree-to-temp-buffer
   "*org-element-skeleton*"
   (org-element-tree-skeleton tree)))

(provide 'koe-skeleton)

;;; koe-skeleton.el ends here
