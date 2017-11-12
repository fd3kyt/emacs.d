;;; koe-parse.el --- Summary

;;; Commentary:
;;
;; name: aoe, anki, org-element
;; no.  too short, may conflict
;; koe for now
;;
;; TODO:
;; bold in list item tag
;; structure in paragraph
;;
;; to-string for each type
;;
;; test with `koe-run-with-first-match'


;;; Code:

;; ########## get dicts
;; (org-element--get-node-properties)  ;; use on headline

(require 'subr-x)
(require 'koe-util)

(defun koe-headline-section (headline)
  "Get the top level section of HEADLINE."
  (cl-assert (eq 'headline (org-element-type headline)))
  (let ((first-child (car (org-element-contents headline))))
    (when (and first-child
               (eq 'section (org-element-type first-child)))
      first-child)))

"Get a dict from lists in tree.

Method 1: find definition lists; find all items in it; create the
dict.

Method 2: Find all items, try to extract a key-value pair from
it.

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
;; (koe-run-with-first-match 'koe--key-value-pair-from-item 'item)
;; (koe-run-with-first-match 'koe--paragraph-string 'paragraph)


"TODO:
bold in list item tag
structure in paragraph
"

(provide 'koe-parse)

;;; koe-parse.el ends here
