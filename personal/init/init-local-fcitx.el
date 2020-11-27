;;; init-local-fcitx.el ---

;;; Commentary:

;;; Code:

;; 使用我的 fcitx3
(defvar kyt/package-dir)

(require 'fcitx3)
(fcitx/enable)

;; ;; ibus
;; (setq fcitx/im-turn-off-command "ibus engine xkb:us::eng"
;;       fcitx/im-turn-on-command "ibus engine rime"
;;       fcitx/im-test-if-turned-on-command "[[ `ibus engine` == rime ]] ")

(provide 'init-local-fcitx)

;;; init-local-fcitx.el ends here
