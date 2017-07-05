;;; init-preload-local.el --- "If you need initialisation code which executes earlier in the startup process, you can also create an ~/.emacs.d/lisp/init-preload-local.el file."

;;; Commentary:
;;

;;; Code:

;; ;; proxy
;; ;; https://www.gnu.org/software/emacs/manual/html_node/url/Proxies.html
;; (require 'url-vars)
;; (setq-default url-proxy-services
;;               ;; dont't use "http://localhost:8123", use "localhost:8123" directly
;;               '(("http"     . "localhost:8123")
;;                 ("https"    . "localhost:8123")
;;                 ;; regular expression, not CSV
;;                 ("no_proxy" . "\\(localhost\\|127.*.*.*\\)")))

(setq url-gateway-method 'socks)
(setq socks-server '("Default server" "127.0.0.1" 4399 5))

(add-to-list 'load-path "~/.emacs.d/site-lisp/benchmark-init-el/")
(require 'benchmark-init-modes)
(benchmark-init/activate)

(provide 'init-preload-local)

;;; init-preload-local.el ends here
