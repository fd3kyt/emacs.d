;;; kyt-ag.el --- A wrapper of ag.el.

;;; Commentary:
;;

;;; Code:


(require 'ag)


(defgroup kyt-ag nil
  "Kyt's wrapper of `ag'."
  :group 'tools
  :group 'matching
  :prefix "kyt-ag-")

(defcustom kyt-ag-base-options (list "--group"
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


(defun kyt-ag-construct-ag-command (options pattern paths)
  "Provide a list of OPTIONS, a PATTERN, a list of PATHS, return ag command."
  (mapconcat 'shell-quote-argument
             (-concat '("ag") options '("--") (list pattern) paths)
             " "))

(kyt-ag-construct-ag-command kyt-ag-base-options "hello world" '("~/.emacs.d" "~/Documents"))


(defun kyt-ag-buffer-name (options pattern paths)
  "Pass OPTIONS, PATTERN, PATHS to `ag/buffer-name' to get ag buffer name."
  (ag/buffer-name pattern (s-join " " paths)
                  (-contains-p options "--literal")))


(defun kyt-ag--run (options pattern paths)
  "Run ag.
OPTIONS, PATTERN, PATHS: see `kyt-ag-construct-ag-command',
and `man' of ag: 'ag [options] pattern [path ...]'"
  (compilation-start
   (kyt-ag-construct-ag-command options pattern paths)
   #'ag-mode
   `(lambda (mode-name) ,(kyt-ag-buffer-name options pattern paths)))
  )

(kyt-ag--run kyt-ag-base-options "peco" (list "/home/fd3kyt/Documents"))

(defun kyt-ag-run (options pattern paths)
  "Run ag with `kyt-ag-base-options' + OPTIONS, PATTERN, PATHS."
  (let ((final-options (append kyt-ag-base-options options)))
    (kyt-ag--run final-options pattern paths)))

(kyt-ag-run nil "peco" (list "/home/fd3kyt/Documents"))


(magit-define-popup kyt-try-magit-popup-8-popup
  "Popup console for `kyt-ag' options."
  'kyt-ag-option-popup
  :actions '("Section 1"
             (?i "Initialize defaults" magit-gitflow-init)
             (?f "Feature prefix"      magit-gitflow-init-feature)
             "Section 2"
             (?u "Run in popup" run-in-popup))
  :switches '((?f "Force reinitialization" "--force")
              (?k "Kaka" "-k"))
  :options '((?t "Timeout" "--time=" read-number))
  :default-arguments '("-k" "--time=200"))


(defun run-in-popup ()
  (interactive)
  (prin1 magit-current-popup-args))


(ag/get-supported-types)



;; todo ag/get-supported-types, ag/read-file-type

;; argument of ag/search, file-regex, file-type

;; https://www.emacswiki.org/emacs/MinibufferHistory

(provide 'kyt-ag)

;;; kyt-ag.el ends here
