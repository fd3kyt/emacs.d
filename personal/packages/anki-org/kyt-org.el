;;; kyt-org.el --- kyt org parsing lib

;;; Commentary:
;;

;;; Code:

;; name: aoe, anki, org-element
;; no. too short, may conflict
;; koe for now

(require 'org-element)
(require 'subr-x)

(defun koe-properties (properties element)
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

(defun koe-print-at-point ()
  "Print `org-element-at-point'."
  (interactive)
  (print-value-to-org-element-buffer
   "*org-element-at-point*"
   (org-element-at-point)))

(defun koe-parse (begin end)
  "Recursively parse region between BEGIN and END."
  (org-element--parse-elements
   begin end
   ;; args come from `org-element-parse-buffer'
   'first-section nil nil nil (list 'org-data nil)))

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

(defun koe-parse-at-point ()
  "Parse at point.  Mainly for the argument of `interactive'."
  (apply 'koe-parse
         (koe-range-at-point)))

(defun koe-print (tree)
  "Print TREE."
  (interactive (list (koe-parse-at-point)))
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
  (print-value-to-org-element-buffer
   "*org-element-skeleton*"
   (org-element-tree-skeleton tree)))

(defun koe-headline-section (headline)
  "Get the top level section of HEADLINE.

   Example usage:   (koe-headline-section (org-element-map
                          (koe-parse-at-point)
                          'headline 'identity nil t))

Note: the output of `koe-parse' is wrapped in
'org-data"
  (cl-assert (eq 'headline (org-element-type headline)))
  (let ((first-child (car (org-element-contents headline))))
    (when (and first-child
               (eq 'section (org-element-type first-child)))
      first-child)))

"Get a dict from lists in tree.

Method 1:
find definition lists; find all items in it; create the dict.

Method 2:
Find all items, try to extract a key-value pair from it.

Method 2 is more simple and has more possibility. (Likes checkbox
to bool)"

(defun koe-dict-from-items (tree)
  "Generate a dict with from lists in TREE."
  (org-element-map tree 'item 'koe--key-value-pair-from-item))

;; (koe-dict-from-items (koe-parse-at-point))

(defun koe--item-tag-string (item)
  "Get the string of ITEM tag.

TODO: support style."
  (when-let ((tag (org-element-property :tag item)))
    (substring-no-properties
     (car tag))))

(defun koe--paragraph-string (paragraph)
  "PARAGRAPH to string.

TODO: support style."
  (cl-assert (eq 'paragraph (org-element-type paragraph)))
  (substring-no-properties
   (car (org-element-contents paragraph))))

(defun koe-map-top-level (data types fun)
  "`org-element-map' on top level contents.
DATA is a parse tree, an element, an object, a string.
TYPES, FUN: same as `org-element-map'."
  (org-element-map (org-element-contents data)
      types fun nil nil org-element-all-elements))

(defun koe--item-paragraph-string (item)
  "Get the string of ITEM paragraphs."
  (apply 'concat
         (koe-map-top-level item 'paragraph
                            'koe--paragraph-string)))

(defun koe--key-value-pair-from-item (item)
  "Extract a key-value-pair from ITEM.  Return nil if failed.

Situations:
If ITEM has a tag, return (tag . paragraph);"
  (cl-assert (eq 'item (org-element-type item)))
  (cond
   ((org-element-property :tag item)
    (cons (koe--item-tag-string item)
          (koe--item-paragraph-string item)))))
;; (koe--test-parsing-fun 'koe--key-value-pair-from-item 'item)


(defun koe--match-types-p (data types)
  "If the type of DATA is one of TYPES.
TYPES can be a list of types or one type."
  (let ((types (if (listp types)
                   types
                 (list types))))
    (-contains-p types
                 (org-element-type data))))

(defun koe--first-at-point (types)
  "Get the first element of TYPES in the parsed tree at point.
For manual testing.

TYPES can be a type or a list of types."
  (org-element-map
      (koe-parse-at-point)
      types 'identity nil t))

(defun koe--test-parsing-fun (fun type &rest args)
  "Call FUN with the first match of TYPE at point.
Pass ARGS to FUN."
  (apply fun (cons (koe--first-at-point type) args)))

;; (koe--test-parsing-fun 'koe--paragraph-string 'paragraph)
;; (koe--test-parsing-fun 'koe--item-paragraph-string 'item)


"TODO:
bold in list item tag
structure in paragraph
"

(provide 'kyt-org)

;;; kyt-org.el ends here
