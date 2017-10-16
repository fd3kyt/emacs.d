;;; kyt-ag.el --- A wrapper of ag.el.

;;; Commentary:
;;

;;; Code:


(require 'ag)


(defgroup kyt-ag nil
  "Kyt's wrapper of `ag'."
  :group 'tools
  :group 'matching
  :prefix "kyt-ag/")

(defcustom kyt-ag/base-options (list "--group"
                                     "--line-number"
                                     "--column"
                                     "--color"
                                     "--color-match" "30;43"
                                     "--color-path" "1;32"
                                     "--smart-case"
                                     "--stats"
                                     "--hidden"
                                     )
  "Base options passed to `ag-executable'.  The default value is copied from `ag'."
  :type 'list
  :group 'kyt-ag)


(defun kyt-ag/construct-ag-command (options pattern paths)
  "Provide a list of OPTIONS, a PATTERN, a list of PATHS, return ag command."
  (mapconcat 'shell-quote-argument
             (-concat '("ag") options '("--") (list pattern) paths)
             " "))

(kyt-ag/construct-ag-command kyt-ag/base-options "hello world" '("~/.emacs.d" "~/Documents"))


(defun kyt-ag/buffer-name (options pattern paths)
  "Pass OPTIONS, PATTERN, PATHS to `ag/buffer-name' to get ag buffer name."
  (ag/buffer-name pattern (s-join " " paths)
                  (-contains-p options "--literal")))


(defun kyt-ag/run (options pattern paths)
  "Run ag.
OPTIONS, PATTERN, PATHS: see `kyt-ag/construct-ag-command',
and `man' of ag: 'ag [options] pattern [path ...]'"
  (compilation-start
   (kyt-ag/construct-ag-command options pattern paths)
   #'ag-mode
   `(lambda (mode-name) ,(kyt-ag/buffer-name options pattern paths)))
  )

(kyt-ag/run kyt-ag/base-options "peco" (list "/home/fd3kyt/Documents"))


;; todo kyt-ag/base-options, kyt-ag/--run


(provide 'kyt-ag)

;;; kyt-ag.el ends here
