;;; init-local-hydra.el --- Initialize hydra for me.

;;; Commentary:
;;

(require 'hydra)

;;; Code:

(defhydra hydra-zoom (:color red)
  "zoom"
  ("I" text-scale-increase "buffer in")
  ("O" text-scale-decrease "buffer out")
  ("i" default-text-scale-increase "in")
  ("o" default-text-scale-decrease "out"))

(defhydra hydra-complete (:color blue)
  "Auto-completion at point."
  ("s" company-ispell "ispell")
  ("d" dabbrev-expand "origin dabbrev" :color red)
  ("D" company-dabbrev "dabbrev")
  ("f" company-files "files")
  ("y" company-yasnippet "yasnippet"))

(defhydra hydra-main (:color blue)
  "main"
  ("z" hydra-zoom/body "zoom")
  ("i" hydra-complete/body "complete"))
(global-set-key (kbd "C-z") 'hydra-main/body)

(provide 'init-local-hydra)

;;; init-local-hydra.el ends here
