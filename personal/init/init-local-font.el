;;; init-local-font.el --- Init the font.

;;; Commentary:
;; note: this file is required in `init-local-main' with
;; `after-make-window-system-frame-hooks'

;;; Code:

;; (declare-function require-package "init-elpa")
;; (require-package 'cnfonts)
;; (require 'cnfonts)          ;still need to require

;; (defun cnfonts-refresh-profile-list ()
;;   "Set `cnfonts-profiles' by finding files under the profile directory."
;;   (interactive)
;;   (let* ((profile-directory (expand-file-name "v4" cnfonts-directory))
;;          (profile-names
;;           (mapcar (lambda (file) (s-chop-suffix ".el" file))
;;                   (directory-files profile-directory
;;                                    nil ".el$"))))
;;     (when (not profile-names)
;;       (error (format "No profiles found in %d" profile-directory)))
;;     (setq cnfonts-profiles profile-names)))

;; ;; adding fonts
;; (setq cnfonts-personal-fontnames
;;       '(("DejaVu Sans Mono" "DejaVuSansMono YaHei NF" "Sarasa Fixed SC")
;;         ("DejaVu Sans Mono" "DejaVuSansMono YaHei NF" "Sarasa Fixed SC")
;;         ("DejaVu Sans Mono" "DejaVuSansMono YaHei NF" "Sarasa Fixed SC")))

;; (cnfonts-refresh-profile-list)
;; (cnfonts-enable)

;; (defvar *is-a-cygwin*)
;; (defvar *is-a-windows*)
;; (cnfonts--select-profile (cond (*is-a-cygwin* "cygwin")
;;                                (*is-a-windows* "windows")
;;                                (t "sarasa")))

;; ;; (when *is-a-cygwin*
;; ;;   (set-frame-font "DejaVuSansMono YaHei NF"))

;; (global-set-key (kbd "C-M--") 'cnfonts-decrease-fontsize)
;; (global-set-key (kbd "C-M-=") 'cnfonts-increase-fontsize)

;; ;; (cnfonts-disable)
;; ;; (cl-prettyprint (x-list-fonts "Monaco"))

;; settings for cnfonts END

(when (featurep 'default-text-scale)
  (default-text-scale-mode -1))
(require 'kyt-font)
(kyt-font/maybe-fallback-font)
(kyt-font/global-font-mode 1)

(provide 'init-local-font)

;;; init-local-font.el ends here
