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

(require 'header2)

(defsubst header-org-mode ()
  "Insert \"#+TITLE: ...\" at the beginning of org file."
  (insert "#+TITLE: " (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "\n") ;; filename only, without dir or extension
  (insert "#+AUTHOR: " (user-full-name) "\n")
  (insert "#+OPTIONS: H:2 num:t toc:nil\n")
  (insert "#+OPTIONS: ^:nil\n")
  (insert "#+OPTIONS: <:nil todo:nil *:t ^:{} @:t ::t |:t TeX:t\n")
  (insert "#how to change image size: #+attr_html: width=\"40%\"\n")
  (yas-insert-snippet))

(add-hook 'org-mode-hook 'auto-make-header)

(add-hook 'make-header-hook 'header-org-mode)



(provide 'init-local-header)
;;; init-local-header.el ends here
