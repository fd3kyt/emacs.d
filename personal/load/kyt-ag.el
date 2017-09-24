;;; kyt-ag.el --- A wrapper of ag.el.

;;; Commentary:
;;

;;; Code:


(require 'ag)

(defun kyt-ag/construct-ag-command (options pattern paths)
  "Provide a list of OPTIONS, a PATTERN, a list of PATHS, return ag command."
  (mapconcat 'shell-quote-argument
             (-concat '("ag") options '("--") (list pattern) paths)
             " "))

(kyt-ag/construct-ag-command '("--cpp" "--literal") "hello world" '("~/.emacs.d" "~/Documents"))


(defun kyt-ag/buffer-name (options pattern paths)
  "Pass OPTIONS, PATTERN, PATHS to `ag/buffer-name' to get ag buffer name."
  (ag/buffer-name pattern (s-join " " paths)
                  (-contains-p options "--literal")))


(defun kyt-ag/run (options pattern paths)
  "OPTIONS, PATTERN, PATHS: see `kyt-ag/construct-ag-command'."
  (compilation-start
   (kyt-ag/construct-ag-command options pattern paths)
   #'ag-mode
   `(lambda (mode-name) ,(ag/buffer-name "search-string" "directory" "regexp")))
  )

(defvar kyt-ag/base-options
  (-map 'shell-quote-argument
        '(
          "--group"
          "--line-number"
          "--column"
          "--color"
          "--color-match" "30;43"
          "--color-path" "1;32"
          "--smart-case"
          "--stats"
          "--hidden"
          )))


(provide 'kyt-ag)

;;; kyt-ag.el ends here
