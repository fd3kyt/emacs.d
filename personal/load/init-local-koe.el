;;; init-local-koe.el --- Summary -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:


(add-to-list 'load-path "/home/XXD/Projects/org2anki/anki-org/")
(require 'koe)

(defvar koe-anki-deck-alias-under-cs nil)
(setq koe-anki-deck-alias-under-cs
      '("cpp" "emacs" "python" "sql" "project" "leetcode"))

(dolist (name koe-anki-deck-alias-under-cs)
  (add-to-list 'koe-anki-deck-alias (cons name (s-concat "cs::" name))))


(provide 'init-local-koe)

;;; init-local-koe.el ends here
