;;; init-local-org.el --- initialize org

;;; Commentary:
;;


;;; Code:
(require-package 'org)

(require-package 'org-download)

(declare-function org-save-all-org-buffers 'org)
(declare-function require-package 'init-elpa)

(defun org-hide-starting-star ()
  "Hide starting star when using org indent."
  (interactive)
  (set-face-background 'org-hide (face-background 'default));hide stars
  (set-face-foreground 'org-hide (face-background 'default)))

(defvar org-startup-indented)
(setq org-startup-indented t)              ;indent

(defun org-save-all-org-buffers-no-message ()
  "Call `org-save-all-org-buffers', clear echo area if in minibuffer."
  (interactive)
  (org-save-all-org-buffers)
  (if (window-minibuffer-p)
      (message nil)))

(defvar org-directory)
(setq org-directory "~/Documents")

(require 'init-local-org-capture)

(define-key org-mode-map
  (kbd "C-M-<return>")
  'org-insert-heading-after-current)

(define-key org-mode-map
  (kbd "M-S-<return>")
  'org-insert-subheading)

;; macro for note
(define-key org-mode-map
  (kbd "C-c z") [?\C-c ?\C-z ?< ?q tab ?\C-y ?\M-q ?\M-> return return])

;; show code highlight
(setq org-src-fontify-natively t)

(defun kyt/org-get-data-directory-name ()
  "Get the expected data directory name of current org file."
  (concat "./"
          (file-name-nondirectory
           (kyt/buffer-file-name-or-default))
          ".d"))

(defvar org-attach-directory)

;; init org-attach
(defun kyt/org-attach-init ()
  "Initialization for org-attach."
  (setq-local org-attach-directory
              (kyt/org-get-data-directory-name)))
(add-hook 'org-mode-hook 'kyt/org-attach-init)

(require-package 'org-download)
(require 'org-download)
(defun kyt/org-screenshot (prefix)
  "Call org-download-screenshot with frame minimized.
PREFIX: if not nil, do not minimize."
  (interactive "P")
  (if prefix
      (org-download-screenshot)
    (progn
      (make-frame-invisible nil t)
      (with-demoted-errors "Error: %S"
        (org-download-screenshot))
      (make-frame-visible))
    ))

(after-load 'org
  (define-key org-mode-map (kbd "C-c M-d")
    'kyt/org-screenshot))

(require 'kyt-lib)

;; TODO This doesn't work. "error in process filter: Symbol’s value as
;; variable is void: link"
(defun kyt/org-download-yank ()
  "`org-download-yank' with proxy."
  (interactive)
  (with-kyt-proxy (org-download-yank)))


(add-hook 'org-mode-hook
          (lambda ()
            ;; same problem as setting kyt/org-attach-init. Maybe I
            ;; should create a function for this.
            (setq org-download-image-dir
                  (kyt/org-get-data-directory-name))
            (advice-add 'org-download--dir-2
                        :filter-return
                        #'kyt/get-reasonable-file-name)))

(setq org-ellipsis "↓")

;; useful tweak

(setq org-tag-persistent-alist
      '(("workflow" . ?w) ("log" . ?l) ("question" . ?q) ("summary" . ?s)) )

(setq org-hide-emphasis-markers nil)

(defun org-refile-goto ()
  "Go to heading using `org-refile'."
  (interactive)
  (org-refile '(4)))
(define-key org-mode-map (kbd "C-\\") 'org-refile-goto)


;; set the faces

(set-face-attribute 'org-list-dt nil
                    :weight 'unspecified
                    :slant 'italic)

(setq org-fontify-quote-and-verse-blocks t)

(let ((dark-color "DimGray"))
  (set-face-attribute 'org-block-begin-line nil
                      :weight 'bold
                      :underline nil
                      :background "#F0F0F0")

  (set-face-attribute 'org-quote nil
                      :background "#F5F2EC"
                      :weight 'normal
                      :foreground dark-color
                      :inherit 'org-block-begin-line)

  (set-face-attribute 'org-block-end-line nil
                      :strike-through "grey"
                      :foreground "grey"
                      :weight 'bold
                      :inherit 'org-quote)

  (set-face-attribute 'org-block nil
                      :foreground dark-color
                      :inherit 'org-quote)

  (set-face-attribute 'org-verse nil
                      :inherit 'org-quote)

  (set-face-attribute 'org-code nil
                      :inherit 'org-block-begin-line)

  (set-face-attribute 'org-verbatim nil
                      :inherit 'org-code)

  (set-face-attribute 'org-meta-line nil
                      :underline dark-color)

  (set-face-attribute 'org-document-title nil  ;low-key
                      :height 1.0))



;;; Babel
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (C . t)
   (shell . t)
   (plantuml . t)
   ))

(defvar org-plantuml-jar-path)
(setq org-plantuml-jar-path "~/local/plantuml.jar")

(defvar plantuml-jar-path)
(setq plantuml-jar-path org-plantuml-jar-path)

;; try org-brain
(require-package 'org-brain)
(require 'org-brain)
(setq org-id-track-globally t)
(setq org-brain-path "~/Documents/")



;; org agenda
(add-to-list 'org-agenda-files "~/Projects/goldfish/")

(setq org-todo-keywords
      '((sequence "INACTIVE(i)" "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
        (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
        (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))

(add-to-list 'org-todo-keyword-faces '("INACTIVE" :inherit font-lock-comment-face))
(add-to-list 'org-todo-keywords-for-agenda #("INACTIVE" 0 1 (idx 8)) t)




;; org-confirm-babel-evaluate
(defvar kyt/org-babel-need-not-confirm nil
  "Languages that don't need confirm before evaluation.")
(setq kyt/org-babel-need-not-confirm '("dot" "plantuml"))

(defun kyt/org-babel-need-confirm-p (language body)
  "Funtion for `org-confirm-babel-evaluate'.
LANGUAGE: language of the code block.
BODY: body of the code block."
  (print language)
  (if (-contains-p kyt/org-babel-need-not-confirm language)
      nil
    t))
(setq org-confirm-babel-evaluate 'kyt/org-babel-need-confirm-p)

(defun kyt/org-refresh-inline-image-if-displaying ()
  "If inline image display is on, refresh inline images."
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))
(add-hook 'org-babel-after-execute-hook 'kyt/org-refresh-inline-image-if-displaying)

(defun kyt/cpp-filename-inidicated-by-code-p (filename code)
  "Return t if FILENAME is indicated in CODE."
  (let ((cpp-scope-operator "::"))
    (and (s-contains-p (s-concat (file-name-base filename) cpp-scope-operator)
                       code)
         (= 1 (s-count-matches cpp-scope-operator code)))))

(defun kyt/org-make-link-description-function (link desc)
  "LINK and DESC: see the manual of `org-make-link-description-function'."
  (let ((link-delimiter "::")
        (new-desc-delimiter " :: "))
    (if (s-contains-p link-delimiter link)
        (let* ((splitted (s-split-up-to link-delimiter link 1))
               (link-path (nth 0 splitted))
               (filename (file-name-nondirectory link-path))
               (link-context (nth 1 splitted)))
          (if (kyt/cpp-filename-inidicated-by-code-p filename link-context)
              link-context  ; optimization for cpp
            (s-concat filename
                      new-desc-delimiter
                      link-context)))
      desc)))

(setq org-make-link-description-function 'kyt/org-make-link-description-function)

;; not done yet
(defun kyt/org-insert-link-other-window (arg)
  "Insert an org link of current position plus two newlines to the other window.
With universal argument ARG, dont't insert newlines."
  (interactive "P")
  (save-window-excursion
    (call-interactively 'org-store-link)
    (other-window 1)
    (org-insert-link nil (car (car org-stored-links)))
    (unless arg
      (newline 2))))


;; Fix: org-follow-link asks for a confirmation for killing temp
;; buffer.
;; reproduce:
;; (with-temp-buffer (org-mode) (insert "hello"))
(defun unset-buffer-modified (&rest arguments)
  "Set `buffer-modified-p' to nil, ignore ARGUMENTS."
  (set-buffer-modified-p nil))
(defun org-supress-kill-confirmation-for-temp-buffer ()
  "Fix: org-follow-link asks for a confirmation for killing temp buffer.
If buffer name is like ' *temp*' or ' *temp*-123' (mind the
space), unset `buffer-modified-p' after changes."
  (when (s-match "^\\s-+\\*temp\\*" (buffer-name))
    (add-hook 'after-change-functions 'unset-buffer-modified t t)))
(add-hook 'org-mode-hook 'org-supress-kill-confirmation-for-temp-buffer)

;; Fix: log note (C-c C-t c), C-c C-c asks for kill confirmation.
(advice-add 'org-store-log-note :before 'unset-buffer-modified)

(require 'org-archive)
(defvar org-archive-default-command)
(setq org-archive-default-command 'org-toggle-archive-tag)

(setq org-refile-targets
      '((nil :maxlevel . 15) (org-agenda-files :maxlevel . 15)))


(setq org-image-actual-width (list 300))


;; <2017-12-30> bug patch: `org-table-align' makes table look weird
;; because of wrong text properties
(defun kyt/remove-org-table-properties (&rest _)
  "Remove text properties of current table."
  (set-text-properties (org-table-begin) (org-table-end) nil))
(advice-add 'org-table-align :after 'kyt/remove-org-table-properties)


(defvar org-clock-heading-function)
(defun kyt/update-org-clock-heading-with-heading-at-point ()
  "Update `org-clock-heading'.  Copy from `org-clock-in'."
  (setq org-clock-heading
        (save-match-data
          (cond ((and org-clock-heading-function
                      (functionp org-clock-heading-function))
                 (funcall org-clock-heading-function))
                ((nth 4 (org-heading-components))
                 (replace-regexp-in-string
                  "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1"
                  (match-string-no-properties 4)))
                (t "???"))))
  (org-clock-update-mode-line))

(defun kyt/org-clock-at-clocking-heading-p ()
  "Return t if at the heading that if currently clocking."
  (and (org-at-heading-p)
       (org-clocking-p)
       org-clock-marker
       (eq (org-base-buffer (current-buffer))
           (marker-buffer org-clock-marker))
       (< (point) org-clock-marker)
       (condition-case-unless-debug _
           (save-excursion
             (eq (with-current-buffer (marker-buffer org-clock-marker)
                   (goto-char org-clock-marker)
                   (org-back-to-heading)
                   (point))
                 (line-beginning-position)))
         (error nil))))

(defun kyt/update-clocking-heading-on-edit (begin &rest _)
  "Update `org-clock-heading' if BEGIN is at the clocking heading."
  (save-excursion
    (goto-char begin)
    (when (kyt/org-clock-at-clocking-heading-p)
      (kyt/update-org-clock-heading-with-heading-at-point))))

(defun kyt/org-capture-udpate-heading-after-change ()
  "Update `org-clock-heading' when editing the clocking heading."
  (add-hook 'after-change-functions 'kyt/update-clocking-heading-on-edit
            nil t))

(add-hook 'org-capture-mode-hook
          'kyt/org-capture-udpate-heading-after-change)


(require 'init-local-org-mobile)

(provide 'init-local-org)

;;; init-local-org.el ends here
