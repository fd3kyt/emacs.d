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

(provide 'init-local-python)

;;; init-local-python.el ends here
