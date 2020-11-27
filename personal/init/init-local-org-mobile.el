;;; init-local-org-mobile.el --- setup mobileorg

;;; Commentary:
;;

;;; Code:

;; setup mobileorg
(require 'org-mobile)
(setq org-mobile-directory "/home/XXD/Mobileorg")
(setq org-mobile-inbox-for-pull (concat org-directory "/index.org"))


;; add recent files into `org-mobile-files' before pushing.

;; Problem: don't want recent files stay in org-mobile-files, because
;; I may want to use `org-mobile-files' directly, and simply remove
;; all files in recent list from `org-mobile-files' is a bad idea.

(defvar recentf-list)
(defun can-be-added-to-org-mobile-files-p (file)
  "If FILE can be added `org-mobile-files'."
  (and (s-ends-with-p ".org" file)
       (file-readable-p file)
       (not (file-directory-p file))
       (not (file-in-directory-p file org-mobile-directory))))

(defun get-final-org-mobile-files ()
  "Decide the `org-mobile-files' used in `kyt/org-mobile-push'."
  (-uniq (append org-mobile-files
                 (-select 'can-be-added-to-org-mobile-files-p
                          recentf-list))))

(defun kyt/org-mobile-push ()
  "Wrap around `org-mobile-push'.

Add recent files into `org-mobile-files' before pushing.

Problem: don't want recent files stay in org-mobile-files,
because I may want to use `org-mobile-files' directly, and simply
remove all files in recent list from `org-mobile-files' is a bad
idea."
  (interactive)
  (let* ((org-directory temporary-file-directory)
         (org-mobile-inbox-for-pull (concat org-directory "/index.org"))
         (org-mobile-files (get-final-org-mobile-files)))
    (org-mobile-push)))


;; TODO
;; clean up Mobileorg/
;; rename for uniqueness
;; recentf, what is "recent"?

(provide 'init-local-org-mobile)

;;; init-local-org-mobile.el ends here
