;;; init-local-hydra.el --- Initialize hydra for me.

;;; Commentary:
;;

(require-package 'hydra)

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

;; https://github.com/abo-abo/hydra/wiki/Flycheck
(defhydra hydra-flycheck
  (:color pink
          :pre (progn (setq hydra-lv t) (flycheck-list-errors))
          :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
          :hint nil)
  "Errors"
  ("f"  flycheck-error-list-set-filter                            "Filter")
  ("n"  flycheck-next-error                                       "Next")
  ("p"  flycheck-previous-error                                   "Previous")
  ("a" flycheck-first-error                                      "First")
  ("e"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q"  nil "QUIT" :color blue))
(global-set-key (kbd "C-!") 'hydra-flycheck/body)

(provide 'init-local-hydra)

;;; init-local-hydra.el ends here
