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

(require 'subr-x)
(require 'koe-util)
(require 'org-eldoc)

;; ########## dict from list
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

(defun koe-headline-top-level-item-dict (headline)
  "Generate a dict from the top level list in HEADLINE."
  (koe-dict-from-items (koe-headline-section headline)))


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

(defun koe--top-level-paragraphs-string (tree)
  "Get the string of TREE top level paragraphs."
  (apply 'concat
         (koe-map-top-level tree 'paragraph
                            'koe--paragraph-string)))

(defun koe--key-value-pair-from-item (item)
  "Extract a key-value-pair from ITEM.  Return nil if failed.

Situations:
If ITEM has a tag, return (tag . paragraph);"
  (cl-assert (eq 'item (org-element-type item)))
  (cond
   ((org-element-property :tag item)
    (cons (koe--item-tag-string item)
          (koe--top-level-paragraphs-string item)))))
;; (koe-run-with-first-match 'koe--key-value-pair-from-item 'item)
;; (koe-run-with-first-match 'koe--paragraph-string 'paragraph)



;; #################### dict from property
;; TODO property inheritance
;; (org-element--get-node-properties)  ;; use on headline
;;
;; Problem: upcase property name
;;
;; Decision: support local properties only.  Case sensitive.
;;
;; How: parse the property-drawer

(defun koe-headline-property-dict (headline)
  "Create a dict of local properties in HEADLINE."
  (org-element-map (koe-headline-section headline)
      'node-property 'koe--key-value-pair-from-node-property))

(defun koe--key-value-pair-from-node-property (node-property)
  "Extract a key-value pair from NODE-PROPERTY."
  (cl-assert (eq 'node-property (org-element-type node-property)))
  (cons (org-element-property :key node-property)
        (org-element-property :value node-property)))


;; #################### dict from subtrees
;; Only support the simplest form.

(defun koe-headline-subtree-dict (headline)
  "Create a dict from the top level subtrees of HEADLINE."
  (koe-map-top-level headline 'headline
                     'koe--key-value-pair-from-headline))

(defun koe--key-value-pair-from-headline (headline)
  "Extract a key-value pair from HEADLINE."
  (cl-assert (eq 'headline (org-element-type headline)))
  (cons (org-element-property :raw-value headline)
        (koe--top-level-paragraphs-string
         (koe-headline-section headline))))

;; #################### `koe-headline-dict' ####################
(defvar koe-headline-dict-functions
  (list 'koe-headline-extra-info-dict
        'koe-headline-property-dict
        'koe-headline-top-level-item-dict
        'koe-headline-subtree-dict)
  "Functions for extracting key-value pairs from a headline.")

(defun koe-headline--dict (headline)
  "Create an alist from HEADLINE with `koe-headline-dict-functions'."
  (apply 'append
         (mapcar (lambda (fun) (apply fun (list headline)))
                 koe-headline-dict-functions)))

(defun koe-normalize-key-value-pair (key-value-pair)
  "Normalize KEY-VALUE-PAIR."
  (cons (car key-value-pair)
        (s-trim (cdr key-value-pair))))

(defun koe-headline-dict (headline)
  "Call `koe-headline--dict' with HEADLINE, normalize the result."
  (mapcar 'koe-normalize-key-value-pair
          (koe-headline--dict headline)))

;; #################### `koe-headline-extra-info' ####################
;; Headline id: now preprocess the whole buffer first. When we get
;; here, we must already have an id.
;;
;; For anki, preprocess. Here, we just skip it if we don't have one.
;; Already done by `koe-headline-property-dict'.

(defvar koe-headline-extra-info-alist
  ;; don't quote functions inside.
  '(("BREADCRUMB" . koe-breadcrumb-of-headline)
    ("FILE" . (lambda (&rest _) (buffer-file-name))))
  ;; TODO tag, ID
  "Alist (KEY . FUN) for `koe-headline-extra-info-dict'.
An alist, KEY is the key name of the info, FUN is called on the
headline to get the value.")

(defun koe-breadcrumb-of-headline (headline)
  "Get the breadcrumb of HEADLINE."
  (save-excursion
    (goto-char (org-element-property :begin headline))
    (org-eldoc-get-breadcrumb)))

(defun koe-headline-extra-info-dict (headline)
  "Return a dict with extra info of HEADLINE."
  (mapcar (lambda (pair)
            (let ((name (car pair))
                  (fun (cdr pair)))
              (cons name
                    (apply fun (list headline)))))
          koe-headline-extra-info-alist))

(provide 'koe-parse)

;;; koe-parse.el ends here
