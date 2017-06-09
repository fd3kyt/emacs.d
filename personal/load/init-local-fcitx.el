;;; init-local-fcitx.el ---

;;; Commentary:
;; https://github.com/cute-jumper/fcitx.el/blob/master/README-zh.org

(require 'fcitx)

(fcitx-aggressive-setup)

(custom-set-variables `(fcitx-use-dbus t))

(provide 'init-local-fcitx)

;;; init-local-fcitx.el ends here