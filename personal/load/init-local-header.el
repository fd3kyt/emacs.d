;;; init-local-header.el --- Initialize header2      -*- lexical-binding: t; -*-

;; Copyright (C) 2017  fd3kyt

;; Author: fd3kyt <fd3kyt@KYT-MintX>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Temp


;;; Code:
(autoload 'yas-expand-snippet "yasnippet")
(auto-insert-mode 1)  ;;; Adds hook to find-files-hook
;; (setq-default auto-insert-query nil) ;;; If you don't want to be prompted before insertion

(defvar kyt/auto-insert-directory "~/.emacs.d/personal/auto-insert/"
  "My `auto-insert' template directory.  Put yasnippet body here.")
(custom-set-variables '(auto-insert-directory kyt/auto-insert-directory))


(defun my-autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(defun kyt/define-auto-insert (condition filename)
  "Simple wrapper of `define-auto-insert'.
CONDITION and FILENAME is passed to `define-auto-insert.'"
  (define-auto-insert condition (vector filename 'my-autoinsert-yas-expand))
  )


;; workflow to add new auto-insert:
;;
;; add a file under `kyt/auto-insert-directory', the file contains the
;; *body* of a yasnippet
;;
;; add a (kyt/define-auto-insert <filename-pattern> <template_file>)
(defvar auto-insert-alist)
(setq auto-insert-alist nil)
(kyt/define-auto-insert "\\.org$" "auto-insert.org")


(provide 'init-local-header)
;;; init-local-header.el ends here