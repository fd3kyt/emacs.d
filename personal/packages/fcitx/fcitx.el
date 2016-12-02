(this-single-command-keys)

(setq fcitx/close-fcitx-sequence '("C-x" "C-c"))

(defun fcitx/vbutlast (vec &optional n)
  "Return a copy of vector VEC with the last N elements removed."
  (vconcat (butlast (append vec nil) n)))

(defun fcitx/inactive-p (key-seq)
  "Return t if fcitx should be inactivate."
  (and (> (length key-seq) 0)
       (or (member key-seq (fcitx/close-fcitx-sequence))
           (fcitx/inactive-p (fcitx/vbutlast key-seq)))))

(let ((key-seq (this-single-command-keys)))
  (if (fcitx/inactive-p key-seq)
      (when (guide-key/update-guide-buffer-p key-seq)
        (guide-key/turn-on-idle-timer))
    (guide-key/close-guide-buffer))
  (setq guide-key/last-key-sequence-vector key-seq))

(defun foo () (interactive) (insert "hello"))
(defadvice foo (after foo-after activate) (insert " world!\n"))
(foo) ;=> hello world!

(defadvice self-insert-command
    (before self-insert-command-before activate)
  (insert " world!\n"))
