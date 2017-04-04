;;; init-local-python.el --- Initialize my python environment

;;; Commentary:
;;

;;; Code:

(require 'python)

;; set these in custom
;; (setq python-shell-interpreter
;;       "/home/fd3kyt/anaconda3/bin/python3")

;; (setq flycheck-python-pylint-executable
;;       "/home/fd3kyt/anaconda3/bin/pylint")

(after-load 'anaconda
  (advice-add 'anaconda-mode-create-response-handler
              :after (lambda (&rest args) (if (window-minibuffer-p)
                                         (message nil)))))

(after-load 'flycheck
  (flycheck-add-next-checker 'python-flake8 'python-pylint))

;; (eval-after-load 'flycheck
;;   '(add-to-list 'flycheck-checkers 'python-pylint))

(require-package 'pyvenv)
(pyvenv-activate "~/anaconda3")

;; convert ipython prompt to python prompt
(fset 'kyt/doctest_ipython_to_python
      "\213\213\C-d>>>\C-n\C-a\213\213\C-d\C-d")

(fset 'kyt/log_marked
      [?\M-w ?\C-\M-b ?\C-\M-f return ?l ?o ?g ?g ?e ?r ?. ?i ?n ?f ?o ?\( ?\" ?\C-y ?: ?  ?% ?s ?\C-f ?, ?  ?\C-y ?\C-e])

(fset 'kyt/log_yank
      "logger.info(\"\C-y: %s\C-f, \C-y\C-e")

(provide 'init-local-python)

;;; init-local-python.el ends here
