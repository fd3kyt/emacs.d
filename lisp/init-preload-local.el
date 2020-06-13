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

;; (setq url-gateway-method 'socks)
;; (setq socks-server '("Default server" "127.0.0.1" 4399 5))

;; ;; https://elpa.emacs-china.org/
;; (setq package-archives '(("melpa" . "http://elpa.emacs-china.org/melpa/")
;;                          ("org" . "http://elpa.emacs-china.org/org/")
;;                          ("gnu"   . "http://elpa.emacs-china.org/gnu/")
;;                          ))

(require-package 'benchmark-init)
(require 'benchmark-init-modes)
(benchmark-init/activate)

(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defmacro unless-linux (&rest body)
  "Run BODY unless it is linux."
  `(unless *is-a-linux*
     ,@body))

(defconst *is-a-cygwin* (eq system-type 'cygwin))
(defmacro unless-cygwin (&rest body)
  "Run BODY unless it is cygwin."
  `(unless *is-a-cygwin*
     ,@body))

(defconst *is-a-windows* (eq system-type 'windows-nt))
(defmacro unless-windows (&rest body)
  "Run BODY unless it is windows."
  `(unless *is-a-windows*
     ,@body))

(provide 'init-preload-local)

;;; init-preload-local.el ends here
