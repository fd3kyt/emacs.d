;;; init-local.el --- Kyt local settings after purcell init

;;; Commentary:
;; 单独运行这个buffer会失败, 而且这里也显示错误信息
;; 但从init.el启动时,会成功
;;

;;; Code:

;; ATTENTION: don't put any code here, before setting the vars.

;; ===== personal global vars =====
(defvar kyt/debug-var)
(set-variable 'kyt/debug-var "start")

;; .emacs.d/personal 作为集中个人设置的地方(相对 purcell 的设置)
(defvar kyt/personal-dir
  (expand-file-name "personal"
                    user-emacs-directory)
  "集中个人设置的地方(相对 purcell 的设置).")

(defvar kyt/package-dir
  (expand-file-name "packages"
                    kyt/personal-dir)
  "放置个人写的类似包的东西.")

(defvar kyt/init-dir
  (expand-file-name "load" kyt/personal-dir)
  "Path to local init dir.")
(add-to-list 'load-path kyt/init-dir)
;; ===== personal global vars end =====

(require 'init-local-main)

(provide 'init-local)
;;; init-local.el ends here
