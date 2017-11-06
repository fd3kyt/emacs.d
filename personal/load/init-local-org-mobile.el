;;; init-local-org-mobile.el --- setup mobileorg

;;; Commentary:
;;

;;; Code:

;; setup mobileorg
(require 'org-mobile)
;; (setq org-mobile-directory "/ssh:root@45.76.55.156:/root/mobileorg")
;; (setq org-mobile-directory "/ssh:ubuntu@111.230.112.173:/home/ubuntu/mobileorg/")
(setq org-mobile-directory "/home/XXD/Mobileorg")
(setq org-mobile-inbox-for-pull (concat org-directory "/index.org"))


;; add recent files into `org-mobile-files' before pushing.

;; Problem: don't want recent files stay in org-mobile-files, because
;; I may want to use `org-mobile-files' directly, and simply remove
;; all files in recent list from `org-mobile-files' is a bad idea.
(defvar org-mobile-files-backup nil
  "Backup of `org-mobile-files' before adding recent org files.")

(defvar recentf-list)
(defun can-be-added-to-org-mobile-files-p (file)
  "If FILE can be added `org-mobile-files'."
  (and (s-ends-with-p ".org" file)
       (file-readable-p file)
       (not (file-directory-p file))
       (not (file-in-directory-p file org-mobile-directory))))
(defun add-recent-org-to-org-mobile-files ()
  "Backup `org-mobile-files', then add recent org files into it."
  (setq org-mobile-files-backup org-mobile-files)
  ;; TODO need copy-list ?
  ;; `append' will copy
  (setq org-mobile-files (append org-mobile-files
                                 (-select 'can-be-added-to-org-mobile-files-p
                                          recentf-list))))
(defun recover-org-mobile-files ()
  "Restore `org-mobile-files'."
  (setq org-mobile-files org-mobile-files-backup))

(add-hook 'org-mobile-pre-push-hook 'add-recent-org-to-org-mobile-files)
(add-hook 'org-mobile-post-push-hook 'recover-org-mobile-files)

;; bug: mobileorg.org
;; remove files in `org-mobile-directory' from `org-mobile-files'

;; TODO refactor, kyt/org-mobile-push

(provide 'init-local-org-mobile)

;;; init-local-org-mobile.el ends here
