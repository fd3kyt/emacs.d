;;; init-local-font.el --- Init the font.

;;; Commentary:
;; note: this file is required in `init-local-main' with
;; `after-make-window-system-frame-hooks'

;;; Code:
(when (featurep 'default-text-scale)
  (default-text-scale-mode -1))
(require 'kyt-font)
(kyt-font/maybe-fallback-font)
(kyt-font/global-font-mode 1)

(provide 'init-local-font)

;;; init-local-font.el ends here
