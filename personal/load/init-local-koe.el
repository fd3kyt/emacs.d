;;; init-local-koe.el --- Summary -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:


(add-to-list 'load-path "/home/XXD/Projects/org2anki/anki-org/")
(require 'paredit)
(require 'koe)

(defvar koe-anki-deck-alias-under-cs nil)
(setq koe-anki-deck-alias-under-cs
      '("cpp" "emacs" "python" "sql" "projects" "leetcode"))

(dolist (name koe-anki-deck-alias-under-cs)
  (add-to-list 'koe-anki-deck-alias (cons name (s-concat "cs::" name))))

(setq koe-anki-model-alias
      `(("basic" . "org:basic")
        ("basic_r" . "org:basic_with_reverse")))

(setq koe-model-dict-alist
      (list (cons "org:basic" 'koe-model-dict-basic)
            (cons "Basic" 'koe-model-dict-basic)
            (cons "org:basic_with_reverse" 'koe-model-dict-basic)))


(defun koe-mark-ankigroup-set-context ()
  "Mark as ankigroup and use title as ANKI_CONTEXT.
Mark current heading as ankigroup and use its title as the value
of ANKI_CONTEXT property."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((tags (-remove (lambda (tag) (s-equals-p tag ""))
                         (org-get-tags)))
          (title (org-element-property :raw-value
                                       (org-element-at-point))))
      (unless (-contains-p tags "@ankigroup")
        (push "@ankigroup" tags))
      (org-set-property koe-dict-context-property-name title)
      (org-set-tags-to tags)
      (save-restriction
        (org-narrow-to-subtree)
        (org-align-all-tags)))))

(add-to-list 'koe-anki-exclude-tags org-archive-tag)
(setq koe-anki-exclude-tags  (append org-export-exclude-tags
                                     koe-anki-exclude-tags))
(cl-delete-duplicates koe-anki-exclude-tags)

(provide 'init-local-koe)

;;; init-local-koe.el ends here
