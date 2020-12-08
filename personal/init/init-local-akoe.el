;;; init-local-akoe.el --- Summary -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar *is-a-linux*)
(add-to-list 'load-path
             (if *is-a-linux*
                 "/home/fd3kyt/Projects/org2anki/akoe/"
               "d:/nutstore/Projects/org2anki/akoe/"))
(require 'paredit)

(require 'akoe)

(setq akoe-anki-model-alias `(("basic" . "org:basic")
                              ("flip" . "org:basic_with_reverse")
                              ("cloze" . "org:cloze")))
(setq akoe-anki-read-model-choices `(("b" . "basic")
                                     ("fr" . "flip")
                                     ("cz" . "cloze")))

(setq akoe-model-dict-alist
      (list (cons "org:basic" 'akoe-model-dict-basic)
            (cons "Basic" 'akoe-model-dict-basic)
            (cons "org:basic_with_reverse" 'akoe-model-dict-basic)
            (cons "org:cloze" 'akoe-model-dict-basic)))

(setq akoe-anki-deck-alias `(("default" . "org::default")))
(let ((decks-under-cs
       '("cpp" "emacs" "python" "sql" "projects" "leetcode")))
  (dolist (name decks-under-cs)
    (add-to-list 'akoe-anki-deck-alias (cons name (s-concat "cs::" name)))))

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
      (org-set-property akoe-default-dict-context-property-name title)
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

;;; when we import notes, add the file into `org-id-extra-files' so
;;; that `org-id' can search this file (e.g. for `akoe-follow'.)
(defvar org-id-extra-files)
(defun akoe--add-file-to-org-id-search-locations ()
  "Add the file of current buffer to org-id search locations."
  (unless (org-agenda-file-p)
    (customize-push-and-save 'org-id-extra-files
                             (list (file-truename (buffer-file-name))))))
;; TODO:
;; (add-hook "need to add ad new hook: success, in original buffer"
;;           'akoe--add-file-to-org-id-search-locations)

(provide 'init-local-akoe)

;;; init-local-akoe.el ends here
