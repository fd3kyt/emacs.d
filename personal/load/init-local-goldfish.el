;;; init-local-goldfish.el --- Settings for goldfish database project of our lab.

;;; Commentary:
;;

;;; Code:

(defvar goldfish/root "/home/74/goldfish")
(defvar goldfish/msg-org-list
  "~/Projects/goldfish/proto_analysis/find_msg/msg.org.list")
(defvar goldfish/msg-list
  "~/Projects/goldfish/proto_analysis/find_msg/msg.intersection.list")

(defvar goldfish/msg-definition-location-file
  "~/Projects/goldfish/proto_analysis/find_msg/msg_definition_location.csv"
  "Expected format: csv, First field is the msg name, others are locations."
  )
(defvar goldfish/msg-definition-location-alist nil)


;; (defun goldfish/highlight ()
;;   (interactive)
;;   (highlight-regexp "\\b\\w+_\\b"))


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

(defun get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun kyt/list-from-string (string)
  "Given STRING, return a list of lines in it, stripped."
  (-map 's-trim (-uniq (s-lines (s-trim string)))))

(defun kyt/list-from-file (file)
  "Given FILE, return a list of lines in it, stripped."
  (kyt/list-from-string (get-string-from-file file)))

;; ^\(MSG_SPG_QE_SIMPLE_PLAN\|MSG_RTABLE_CS_CS_SEARCH_ACK\)$
(defun kyt/regexp-for-any-line (string)
  "Given STRING, return a regexp that match any line in it."
  (s-concat "^\\("
            (s-join "\\|" (-map 's-trim (-uniq (s-lines (s-trim string)))))
            "\\)$"))

(defun kyt/highlight-in (file)
  "Highlight all lines that are in FILE lines."
  (interactive (list (read-file-name "The list file: ")))
  (highlight-regexp
   (kyt/regexp-for-any-line (get-string-from-file file))
   'hi-yellow)
  )

(defun goldfish/msg-list-highlight ()
  "Set highlight in msg.intersetion.list."
  (interactive)
  (unhighlight-regexp t)
  (kyt/highlight-in goldfish/msg-org-list)
  (highlight-regexp "_ACK$" 'hi-pink))


(defun goldfish/read-msg-name ()
  "Read a msg name from minibuffer, with auto-completion."
  (completing-read "MSG: " (kyt/list-from-file goldfish/msg-list)))

(defun goldfish/read-msg-definition-location-file (file)
  "Read `goldfish/msg-definition-location-file' from FILE."
  (-map (lambda (s) (s-split "," s))
        (s-lines (get-string-from-file file))))

(defun goldfish/setup ()
  "Setup goldfish variables."
  (setq goldfish/msg-definition-location-alist
        (goldfish/read-msg-definition-location-file
         goldfish/msg-definition-location-file)))

(defun goldfish/query-msg-definition-location-alist (msg)
  "Given MSG, return its locations by querying `goldfish/msg-definition-location-alist'."
  (cdr (assoc msg goldfish/msg-definition-location-alist)))

(defun goldfish/jump-to-msg-definition (msg &optional arg)
  "Given MSG, jump to its definition.  Prefix arg ARG.
With an universal argument, choose from all available locations."
  (interactive (list (goldfish/read-msg-name)
                     current-prefix-arg))
  (org-open-link-from-string
   (let ((locations (goldfish/query-msg-definition-location-alist msg)))
     (s-concat "file:"
               (if (equal arg '(4))
                   (completing-read "Location:" locations)
                 (car locations))))
   )
  )




(goldfish/setup)

(provide 'init-local-goldfish)

;;; init-local-goldfish.el ends here
