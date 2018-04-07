;;; init-local-koe.el --- Summary -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:


(add-to-list 'load-path "/home/XXD/Projects/org2anki/anki-org/")
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


(provide 'init-local-koe)

;;; init-local-koe.el ends here
