;;; init-preload-local.el --- "If you need initialisation code which executes earlier in the startup process, you can also create an ~/.emacs.d/lisp/init-preload-local.el file."

;;; Commentary:
;;

;;; Code:

;; proxy
;; https://www.gnu.org/software/emacs/manual/html_node/url/Proxies.html
(setq-default url-proxy-services
              '(("http"     . "http://localhost:8123")
                ("https"    . "http://localhost:8123")
                ("no_proxy" . "\\(localhost\\|127.*.*.*\\)")))

(provide 'init-preload-local)

;;; init-preload-local.el ends here
