;;; kyt-lib.el --- Util for kyt

;;; Commentary:
;;

;;; Code:

(defun kyt/get-reasonable-file-name (string)
  "Replace special characters in STRING, make it a valid file name."
  (replace-regexp-in-string "[: *&^%$#@!?\"'/]" "_"
                            string))

(defcustom kyt/temp-file-base-name  (concat (substitute-in-file-name "$HOME/")
                                            "emacs-temp/")
  "Default file base name of kyt/buffer-file-name-or-default."
  :type 'string
  :group 'kyt-lib)

(defun kyt/buffer-file-name-or-default ()
  "Return `w/buffer-file-name'.
If it is nil, a make temp name with kyt/temp-file-base-name as base name."
  (unless (buffer-file-name)
    (setq-local buffer-file-name
                (make-temp-name kyt/temp-file-base-name)))
  (buffer-file-name))

;;; vertical editing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kyt/move-to-goal-column ()
  "Move to `goal-column' if it is setted, add space if line is too short."
  (if goal-column
      (move-to-column goal-column t)))

(defun kyt/goal-column-next-line (&optional ARG TRY-VSCROLL)
  "Next line and `kyt/move-to-goal-column'.
Pass `ARG' and `TRY-VSCROLL' to `next-line'."
  (interactive "P")
  (next-line ARG TRY-VSCROLL)
  (kyt/move-to-goal-column))

(defun kyt/goal-column-previous-line (&optional ARG TRY-VSCROLL)
  "Previous line and `kyt/move-to-goal-column'.
Pass `ARG' and `TRY-VSCROLL' to `previous-line'."
  (interactive "P")
  (previous-line ARG TRY-VSCROLL)
  (kyt/move-to-goal-column))

(define-minor-mode kyt/vertical-editing
  "Get your foos in the right places."
  :lighter " vert"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-n")
              'kyt/goal-column-next-line)
            (define-key map (kbd "C-p")
              'kyt/goal-column-previous-line)
            map))


;; TODO weird, start-process don't work with xdg-open.
;; shell-command works, but blocks.
(defun kyt/open-current-file ()
  "Open current file with the default program."
  (interactive)
  (if buffer-file-name
      (start-process "External File" nil
                     "xdg-open"
                     buffer-file-name)
    (message "Current buffer is not a file!")))

(defun kyt/open-current-image-viewnior ()
  "Open current image with viewnior."
  (interactive)
  (if buffer-file-name
      (start-process "Viewnior" nil
                     "viewnior"
                     buffer-file-name)
    (message "Current buffer is not a file!")))

(defun kyt/open-current-image-shutter ()
  "Open current image with viewnior."
  (interactive)
  (if buffer-file-name
      (start-process "Shutter" nil
                     "shutter"
                     buffer-file-name)
    (message "Current buffer is not a file!")))

;;; TODO make use of C-c l instead
(defun kyt/copy-buffer-file-name (&optional nondirectory link)
  "Add buffer file name to kill ring.
With `universal-argument' NONDIRECTORY, only the nondirectory part.
If LINK is non-nil, add link prefix.

TODO: prefix 1,2,3 for name,dir,path like Total Commander."
  (interactive "P\ni")
  (if (buffer-file-name)
      (let ((stored (s-concat (if link "file:" "")
                              (if nondirectory
                                  (file-name-nondirectory (buffer-file-name))
                                (buffer-file-name)))))
        (kill-new stored)
        (message "Copied: %s" stored))
    (message "Current buffer is not a file.")))

(defun kyt/copy-buffer-link ()
  "Copy file link."
  (interactive)
  (kyt/copy-buffer-file-name nil t))

(defun kyt/new-line ()
  "Create a new line under current line and go to it."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))


(defun kyt/go-to-beginning-and-search ()
  "Go to the beginning of current buffer and start isearch."
  (interactive)
  (goto-char (point-min))
  (isearch-forward))


;; copy from http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defvar kyt/common-delimiters '(";" "," " " "/" ":")
  "Commonly used delimiters.")

(defun kyt/decide-delimiter (string)
  "Given STRING, try to decide its delimiter.
Try the delimiters in `kyt/common-delimiters', count occurences."
  (car (-max-by
        (lambda (left right)
          (> (cdr left) (cdr right)))
        (mapcar (lambda (delimiter) (cons delimiter (s-count-matches delimiter string)))
                kyt/common-delimiters))))

(defun kyt/read-list ()
  "Read a list from the user.
Read a string of filenames, then read a delimiter to split it."
  (let ((string (read-string "Filenames: ")))
    (let ((delimiter (read-string "Delimiter: "
                                  (kyt/decide-delimiter string))))
      (split-string string delimiter)))
  )

(declare-function dired-mark-files-regexp "dired")
(defun kyt/dired-mark-list (list)
  "Mark all file names in LIST."
  (interactive (list (kyt/read-list)))
  (dired-mark-files-regexp
   (concat "^\\(" (s-join "\\|" list) "\\)$")
   nil t))


;;; end of vertical editing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TODO move to a right place
(defun kyt/batch-replace (begin end replace)
  "Between BEGIN and END, relpace according REPLACE.

REPLACE can be a string or a list of two strings."
  (dolist (var replace)
    (if (listp var)
        (replace-string (car var) (cdr var) nil begin end)
      (replace-string var "" nil begin end))))

(defun kyt/print-object-to-temp-buffer (object)
  "Insert OBJECT to a temp buffer and show it."
  (with-current-buffer (get-buffer-create "*temp-object*")
    (erase-buffer)
    (pp object (current-buffer))
    (display-buffer (current-buffer))
    nil))

(defun kyt/print-string-to-temp-buffer (string)
  "Insert STRING to a temp buffer and show it."
  (with-output-to-temp-buffer "*temp-string*"
    (princ string))
  nil)


;; (require 'kyt-ag)
(require 'kyt-avy)


(defun kyt/visible-buffers ()
  "Return a list of visible buffers.  Copy from rtags.el."
  (let (buffers)
    (dolist (frame (frame-list))
      (dolist (window (window-list frame))
        (cl-pushnew (window-buffer window) buffers)))
    buffers))


(defun kyt/warn-when-error (fun &rest args)
  "Run FUN with ARGS show warning (and rethrow) when error."
  (condition-case err
      (apply fun args)
    (error
     (display-warning 'warn-when-error
                      (format "Error when running %S"
                              (cons fun args)))
     (signal (car err) (cdr err)))))
;; (kyt/warn-when-error 'signal 'error (list "errbody"))


(provide 'kyt-lib)

;;; kyt-lib.el ends here
