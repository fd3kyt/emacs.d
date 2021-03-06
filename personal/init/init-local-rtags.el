;;; init-local-rtags.el --- Initialize rtags for me.

;;; Commentary:
;; Most of these is from rtags README.
;;

;;; Code:
(require-package 'rtags)
(require 'rtags)

;; (setq rtags-path "~/Sources/rtags/bin")
;; "sudo make install", don't need to set the path.

(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)


(rtags-enable-standard-keybindings)


(require-package 'company)
(require-package 'company-rtags)
(require-package 'flycheck-rtags)
(require-package 'ivy-rtags)

;; auto completion
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(global-company-mode)
(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))


(require-package 'flycheck-rtags)
;; flycheck
(defun my-flycheck-rtags-setup ()
  (unless (eq major-mode 'java-mode)
    (require 'flycheck-rtags)
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil)  ;; RTags creates more accurate overlays.
    (setq-local flycheck-check-syntax-automatically nil)))
;; c-mode-common-hook is also called by c++-mode
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)


;; ivy
(setq rtags-display-result-backend 'ivy)


;; key bindings
(defun kyt/cpp-goto (&optional prefix)
  (interactive "P")
  (if (rtags-is-indexed)
      (rtags-find-symbol-at-point prefix)
    (message "Project not indexed. Fallback to xref.")
    (call-interactively 'xref-find-definitions)))  ; TODO useless prompt?

(defun kyt/cpp-find-references (&optional prefix)
  (interactive "P")
  (if (rtags-is-indexed)
      (rtags-find-references-at-point prefix)
    (xref-find-references)))

(defun kyt/cpp-back ()
  (interactive)
  (if (rtags-is-indexed)
      (rtags-location-stack-back)
    (xref-pop-marker-stack)))


(define-key c-mode-base-map (kbd "M-.") (function kyt/cpp-goto))
(define-key c-mode-base-map (kbd "M-,") (function kyt/cpp-back))

(defun do-not-elide (oldfun &rest r)
  "Call OLDFUN with arguments R, having `rtags-elide-text' do nothing."
  (cl-letf (((symbol-function 'rtags-elide-text)
             (lambda (str &rest args)
               str)))
    (apply oldfun r)))

(advice-add 'rtags-insert-ref :around 'do-not-elide)


(set-face-attribute 'rtags-skippedline nil
                    :background "#F5F2EC")


(provide 'init-local-rtags)



;;; init-local-rtags.el ends here
