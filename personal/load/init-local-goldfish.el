;;; init-local-goldfish.el --- Settings for goldfish database project of our lab.

;;; Commentary:
;;

;;; Code:

(defvar goldfish/root "/home/74/goldfish")


(defun goldfish/highlight ()
  (interactive)
  (highlight-regexp "\\b\\w+_\\b"))


(require 'ag)
(defun goldfish/ag-cpp-no-pb (string directory)
  "Ag, cpp only, ignore files compiled from protobuf.

STRING, DIRECTORY: same as `ag-files'."
  (interactive (list (read-string "String: ")
                     (read-directory-name "Directory: ")))
  (let ((ag-arguments (append ag-arguments '("--ignore" "*.pb.cc" "--ignore" "*.pb.h"))))
    (ag-files string '(:file-type "cpp") directory))
  )

(defun goldfish/ag-root (string)
  "Ag cpp for STRING in the whole goldfish directory."
  (interactive (list (read-string "String: ")))
  (let ((ag-arguments (append ag-arguments '("--ignore" "tag1.5_20170410"))))
    (goldfish/ag-cpp-no-pb string goldfish/root)))

(defun goldfish/ag-org (string regexp-p)
  "Ag STRING in ~/Projects/goldfish/*.org.

If called with a prefix, use regexp (REGEXP-P will be t).

TODO: copy `kyt/ag-project-org'."
  (interactive (let ((regexp-p (> (prefix-numeric-value current-prefix-arg) 1)))
                 (list (read-string (if regexp-p "Regexp: " "String: "))
                       regexp-p)))
  (let ((current-prefix-arg 1))         ;avoid passing down to `ag/search'
    (let ((ag-arguments (append ag-arguments '("-U"))))
      (ag/search string "~/Projects/goldfish/"
                 :regexp regexp-p :file-regex "\\.org$"))
    )
  )


(provide 'init-local-goldfish)

;;; init-local-goldfish.el ends here
