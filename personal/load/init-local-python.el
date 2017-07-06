;;; init-local-python.el --- Initialize my python environment

;;; Commentary:
;;

;;; Code:

(require-package 'python)

;; set these in custom
;; (custom-set-variables `(python-shell-interpreter
;;                         "/home/fd3kyt/anaconda3/bin/python3"))

;; (custom-set-variables `(flycheck-python-pylint-executable
;;                         "/home/fd3kyt/anaconda3/bin/pylint"))

(defun kyt/restart_anaconda_if_error (&optional ignored)
  "Restart anaconda if *anaconda-mode* contain traceback.

`IGNORED': passed by `company-completion-started-hook'."
  (when (get-buffer "*anaconda-mode*")
    (with-current-buffer "*anaconda-mode*"
      (when (string-match-p (regexp-quote "Traceback") (buffer-string))
        (message "Restart anaconda server...")
        (kill-buffer)))
    )
  )

(after-load 'company-anaconda
  (add-hook 'company-completion-started-hook 'kyt/restart_anaconda_if_error)
  )


(after-load 'anaconda
  (advice-add 'anaconda-mode-create-response-handler
              :after (lambda (&rest args) (if (window-minibuffer-p)
                                         (message nil))))
  ;; avoid hiding ag-project
  (define-key anaconda-mode-map (kbd "M-?") nil)
  (remove-hook 'anaconda-mode-process-fail-hook 'anaconda-mode-show-process-buffer)
  (add-hook 'anaconda-mode-process-fail-hook 'anaconda-mode-start)
  (add-hook 'anaconda-mode-response-read-fail-hook 'anaconda-mode-start)
  ;; (add-hook 'anaconda-mode-hook 'kyt/register-anaconda-autorestart)
  )

;; (after-load 'flycheck
;;   (flycheck-add-next-checker 'python-flake8 'python-pylint))

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
