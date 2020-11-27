;;; init-local.el --- KYT local settings after purcell's init

;;; Commentary:
;;

;;; Code:

;; ATTENTION: don't put any code here, before setting the vars.

;; ===== basic personal global vars =====

;;; .emacs.d/personal
(defvar kyt/personal-dir (expand-file-name "personal" user-emacs-directory)
  "My settings, seperated from purcell's.")
;;; .emacs.d/personal/packages
(defvar kyt/package-dir (expand-file-name "packages" kyt/personal-dir)
  "My packages.")
;;; .emacs.d/personal/init
(defvar kyt/init-dir (expand-file-name "init" kyt/personal-dir)
  "Path to local init dir.")

(add-to-list 'load-path kyt/init-dir)
;;; add non-hidden direct subdirs of `kyt/package-dir' to `load-path'
(sanityinc/add-subdirs-to-load-path kyt/package-dir)

;; ===== basic personal global vars end =====

(require 'init-local-main)

(provide 'init-local)
;;; init-local.el ends here
