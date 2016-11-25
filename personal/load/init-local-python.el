;;; init-local-python.el --- Initialize my python environment

;;; Commentary:
;;

;;; Code:

(require 'python)

(setq python-shell-interpreter
      "/home/fd3kyt/anaconda3/bin/python3")

(advice-add 'anaconda-mode-create-response-handler
            :after (lambda (&rest args) (if (window-minibuffer-p)
                                       (message nil))))

(provide 'init-local-python)

;;; init-local-python.el ends here
