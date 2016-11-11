;;; init-preload-local.el --- "If you need initialisation code which executes earlier in the startup process, you can also create an ~/.emacs.d/lisp/init-preload-local.el file."

;;; Commentary:
;;

;;; Code:

;; proxy
(setq-default url-proxy-services
              '(("http"     . "http://localhost:8123")
                ("https"    . "http://localhost:8123")
                ("no_proxy" . "^.*example.com")))

(provide 'init-preload-local)

;;; init-preload-local.el ends here
