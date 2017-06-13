;;; init-local-fcitx.el ---

;;; Commentary:

;;; Code:

;; 使用我的 fcitx3
(defvar kyt/package-dir)
(add-to-list 'load-path
             (expand-file-name "fcitx"
                               kyt/package-dir))
(require 'fcitx3)
(fcitx/enable)

(provide 'init-local-fcitx)

;;; init-local-fcitx.el ends here
