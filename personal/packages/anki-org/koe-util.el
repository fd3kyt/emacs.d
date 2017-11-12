;;; koe-util.el --- Utility for org-element parse tree.

;;; Commentary:
;;

;;; Code:


(require 'org-element)
(require 'koe-tree-mode)

(defun koe-properties (properties element)
  "Extract the values from the PROPERTIES of an ELEMENT."
  (mapcar (lambda (property)
            (org-element-property property element))
          properties))

(defun koe-range-at-point ()
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

(defun koe-match-types-p (data types)
  "If the type of DATA is one of TYPES.
TYPES can be a list of types or one type."
  (let ((types (if (listp types)
                   types
                 (list types))))
    (-contains-p types
                 (org-element-type data))))

(defun koe-map-top-level (data types fun)
  "`org-element-map' on top level contents.
DATA is a parse tree, an element, an object, a string.
TYPES, FUN: same as `org-element-map'."
  (org-element-map (org-element-contents data)
      types fun nil nil org-element-all-elements))

;; #################### Parse and Print ####################

(defun koe-parse (begin end)
  "Recursively parse region between BEGIN and END."
  (org-element--parse-elements
   begin end
   ;; args come from `org-element-parse-buffer'
   'first-section nil nil nil (list 'org-data nil)))

(defun koe-parse-at-point ()
  "Parse at point.  Mainly for the argument of `interactive'."
  (apply 'koe-parse
         (koe-range-at-point)))

(defun koe-print (tree)
  "Print TREE."
  (interactive (list (koe-parse-at-point)))
  (print-parse-tree-to-temp-buffer "*org-element-parsed*" tree))

;; #################### Test utility ####################
(defun koe-first-at-point (types)
  "Get the first element of TYPES in the parsed tree at point.
For manual testing.

TYPES can be a type or a list of types."
  (org-element-map
      (koe-parse-at-point)
      types 'identity nil t))

(defun koe-run-with-first-match (fun type &rest args)
  "Call FUN with the first match of TYPE at point.
Pass ARGS to FUN.

Frequently I write a function for a specific type.  I want to
test it with the first match of this type, start from current
point."
  (apply fun (cons (koe-first-at-point type) args)))

(provide 'koe-util)

;;; koe-util.el ends here
