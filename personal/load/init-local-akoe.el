;;; init-local-akoe.el --- Summary -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:


(add-to-list 'load-path
             "/home/fd3kyt/Projects/org2anki/akoe/")
(require 'paredit)

(require 'akoe)

(defvar akoe-anki-deck-alias-under-cs nil)
(setq akoe-anki-deck-alias-under-cs
      '("cpp" "emacs" "python" "sql" "projects" "leetcode"))

(dolist (name akoe-anki-deck-alias-under-cs)
  (add-to-list 'akoe-anki-deck-alias (cons name (s-concat "cs::" name))))

(setq akoe-anki-model-alias
      `(("basic" . "org:basic")
        ("basic_r" . "org:basic_with_reverse")
        ("cloze" . "org:cloze")))

(setq akoe-model-dict-alist
      (list (cons "org:basic" 'akoe-model-dict-basic)
            (cons "Basic" 'akoe-model-dict-basic)
            (cons "org:basic_with_reverse" 'akoe-model-dict-basic)
            (cons "org:cloze" 'akoe-model-dict-basic)))


(defun akoe-mark-ankigroup-set-context ()
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
      (org-set-property akoe-dict-context-property-name title)
      (org-set-tags tags)
      (save-restriction
        (org-narrow-to-subtree)
        (org-align-tags t))))
  (save-buffer))

(setq akoe-anki-exclude-tags
      (append (list org-archive-tag) org-export-exclude-tags))
(cl-delete-duplicates akoe-anki-exclude-tags)

(add-to-list 'akoe-anki-exclude-todo-states "TODO")
(add-to-list 'akoe-anki-exclude-todo-states "CANCELLED")

(provide 'init-local-akoe)

;;; init-local-akoe.el ends here
