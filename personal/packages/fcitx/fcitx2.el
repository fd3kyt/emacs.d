;; (defun foo () (interactive) (insert "hello"))
;; (defadvice foo (after foo-after activate) (insert " world!\n"))
;; (foo) ;=> hello world!

(defun fcitx/get-state ()
  (string-to-number
   (substring
    (shell-command-to-string "fcitx-remote") 0 -1)))

;; TODO global or local
(defvar fcitx/current-state
  "fcitx current state from fcitx-remote shell command")

(defun fcitx/save-state ()
  (setq fcitx/current-state (fcitx/get-state)))

(defadvice self-insert-command
    (before self-insert-command-before activate)
  (insert " world!\n"))
