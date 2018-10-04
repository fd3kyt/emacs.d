;;; init-local-font.el --- Init the font.

;;; Commentary:
;;

;;; Code:

(declare-function require-package "init-elpa")
(require-package 'cnfonts)
(require 'cnfonts)          ;still need to require

(defun cnfonts-refresh-profile-list ()
  "Set `cnfonts-profiles' by finding files under the profile directory."
  (interactive)
  (let* ((profile-directory (expand-file-name "v4" cnfonts-directory))
         (profile-names
          (mapcar (lambda (file) (s-chop-suffix ".el" file))
                  (directory-files profile-directory
                                   nil ".el$"))))
    (when (not profile-names)
      (error (format "No profiles found in %d" profile-directory)))
    (setq cnfonts-profiles profile-names)))

;; adding fonts
(setq cnfonts-personal-fontnames
      '(("DejaVu Sans Mono")
        ("DejaVu Sans Mono")
        ("DejaVu Sans Mono")))

(cnfonts-refresh-profile-list)
(cnfonts-enable)
(cnfonts--select-profile "dejavu")


(global-set-key (kbd "C-M--") 'cnfonts-decrease-fontsize)
(global-set-key (kbd "C-M-=") 'cnfonts-increase-fontsize)

(provide 'init-local-font)

;;; init-local-font.el ends here
