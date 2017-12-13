;;; kyt-proxy.el --- Summary -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:


(require 'request)
(require 'url-vars)

(defvar kyt-proxy-host "localhost")
(defvar kyt-proxy-port "8123")

(defmacro with-kyt-proxy (&rest body)
  "Execute BODY with proxy settings."
  `(let ((url-proxy-services
          '(("http"     . (format "%s:%s" kyt-proxy-host kyt-proxy-port))
            ("https"    . (format "%s:%s" kyt-proxy-host kyt-proxy-port))
            ;; regular expression, not CSV
            ("no_proxy" . "\\(localhost\\|127.*.*.*\\)")))
         (process-environment (copy-sequence process-environment)))
     (setenv "http_proxy" (format "http://%s:%s" kyt-proxy-host kyt-proxy-port))
     (setenv "https_proxy" (format "http://%s:%s" kyt-proxy-host kyt-proxy-port))
     ,@body))

(defun test-with-kyt-proxy ()
  "Test `with-kyt-proxy'."
  (with-kyt-proxy
   (request "http://ip111.cn/"
            :parser (lambda ()
                      (let ((exp "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+")
                            result)
                        (while (re-search-forward exp nil t)
                          (push (match-string 0) result))
                        (reverse result)))
            :success (cl-function
                      (lambda (&key data &allow-other-keys)
                        (message "I get: %S" data))))))



(provide 'kyt-proxy)

;;; kyt-proxy.el ends here
