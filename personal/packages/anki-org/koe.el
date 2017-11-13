;;; koe.el --- Summary

;;; Commentary:
;;

;;; Code:

(defvar kyt/package-dir)
(add-to-list 'load-path
             (expand-file-name "anki-org"
                               kyt/package-dir))
(require 'koe-util)
(require 'koe-skeleton)
(require 'koe-tree-mode)

;; TODO
;; when org is not saved yet

(provide 'koe)

;;; koe.el ends here
