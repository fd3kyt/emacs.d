;;; fcitx3.el ---
;; TODO global or local

;;; Commentary:
;;

;; TODO 隐藏两个 was 变量(循环 + wait)

;;; Code:

(defvar fcitx/typeable-checking-key (kbd "a")
  "the key (usually a letter) used to check if we can type in the
  buffer now.")

;; (setq fcitx/typeable-checking-key (kbd "a"))

(defvar fcitx/allow-p t)

(defun fcitx/get-testing-key-binding ()
  ;; 针对按住鼠标的处理
  (let ((key-seq (remove-if-not 'characterp
                                (this-single-command-keys))))
    (key-binding
     (concat key-seq
             fcitx/typeable-checking-key))))

(defun fcitx/typeable-p ()
  (let ((kb (fcitx/get-testing-key-binding)))
    ;; (key-binding (kbd "C-x a")) return a keymap
    (and (symbolp kb)
         ;; for things like org-self-insert-command
         (string-suffix-p "self-insert-command"
                          (symbol-name kb)))))

(defvar fcitx/debug-timer nil
  "debug timer")

(setq fcitx/debug-timer nil)

(defun fcitx/show_debug_info ()
  (message
   (concat (if (fcitx/permitted-p)
               "fcitx permittedd."
             "fcitx not permittedd.")
           "\tkey-binding:\t"
           (prin1-to-string
            (fcitx/get-testing-key-binding)))))

(defun fcitx/start-debug-timer ()
  (interactive)
  (fcitx/stop-debug-timer)
  (setq fcitx/debug-timer
        (run-at-time t 1 'fcitx/show_debug_info)))

(defun fcitx/stop-debug-timer ()
  (interactive)
  (if fcitx/debug-timer
      (cancel-timer fcitx/debug-timer))
  (setq fcitx/debug-timer nil))


;;; set the state of fcitx automatically

(defvar fcitx/polling-interval 0.1
  "Time interval to execute polling function.")

(defvar hydra-curr-map)

(defun fcitx/permitted-p ()
  "Check if fcitx should be turned on."
  (and (not buffer-read-only)
       (fcitx/typeable-p)
       (not (window-minibuffer-p))
       (not (and (boundp 'hydra-curr-map) hydra-curr-map))
       (not (last-char-is-a-letter-p))
       (not (after-chinese-and-space-p))))

;; TODO refactor
(defun last-char-is-a-letter-p ()
  "Tell if last character is a english letter.

https://emacs.stackexchange.com/questions/8261/how-to-determine-if-the-current-character-is-a-letter"
  (when (char-before)
    (memq (get-char-code-property (char-before) 'general-category)
          '(Ll Lu))))

(defun after-chinese-and-space-p ()
  "Tell if after chinese and a space."
  (when (char-before (- (point) 1))
    (and (memq (get-char-code-property (char-before (- (point) 1))
                                       'general-category)
               '(Lo))
         (= (char-before) ? ))))

  (defun last-command-is-a-self-insert-command-p ()
    "Tell if COMMAND a self-insert command."
    (string-suffix-p "self-insert-command"
                     (symbol-name last-command)))

(defvar fcitx/last-polling-permitted-p nil
  "the result of fcitx/permitted-p() in last polling")

(defvar fcitx/was-active-p nil
  "if fcitx was active before it becomed not permitted")

(defun fcitx/home-dir-shell-command (command)
  (let ((default-directory "~/"))
    (call-process "bash" nil nil nil
                  "-c" command)))

(defcustom fcitx/im-turn-on-command "fcitx-remote -o"
  "Command to acitvate the input method."
  :group 'fcitx)

(defcustom fcitx/im-turn-off-command "fcitx-remote -c"
  "Command to deacitvate the input method."
  :group 'fcitx)

(defcustom fcitx/im-test-if-turned-on-command "[[ `fcitx-remote` == 2 ]]"
  "Command to deacitvate the input method."
  :group 'fcitx)

;; (defun fcitx/current-active-p ()
;;   (eq 2
;;       (string-to-number
;;        (fcitx/home-dir-shell-command "fcitx-remote"))))

(defun fcitx/current-active-p ()
  (eq 0 (fcitx/home-dir-shell-command fcitx/im-test-if-turned-on-command)))

(defun fcitx/polling-function ()
  (let ((current-permitted (fcitx/permitted-p)))
    ;; 边缘触发
    (unless (eq fcitx/last-polling-permitted-p
                current-permitted)
      (when (and current-permitted
                 fcitx/was-active-p)
        (fcitx/home-dir-shell-command fcitx/im-turn-on-command))
      (when (not current-permitted)
        (when (setq fcitx/was-active-p
                    (fcitx/current-active-p))
          (fcitx/home-dir-shell-command fcitx/im-turn-off-command))))
    (setq fcitx/last-polling-permitted-p current-permitted)))

(defvar fcitx/polling-timer nil
  "polling timer")

(defun fcitx/enable ()
  (interactive)
  (fcitx/disable)
  (setq fcitx/polling-timer
        (run-at-time t fcitx/polling-interval
                     'fcitx/polling-function)))

(defun fcitx/disable ()
  (interactive)
  (if fcitx/polling-timer
      (cancel-timer fcitx/polling-timer))
  (setq fcitx/polling-timer nil))

(provide 'fcitx3)


;;; fcitx3.el ends here
