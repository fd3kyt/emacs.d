;;; init-local-simple-key-bindings.el --- Simple global key bindings, which don't need to load extra packages and are usually one-liners.

;;; Commentary:
;;


;;; Code:

(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim)

(global-set-key (kbd "C-S-t") 'transpose-words)
(global-set-key (kbd "M-t") 'transpose-sexps)

(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-M-z") 'avy-goto-char-in-line)
(global-set-key (kbd "C-S-y") 'kyt/avy-copy-line-here)
(global-set-key (kbd "C-c L") 'kyt/avy-insert-line-link)

(global-set-key (kbd "C-j") 'kyt/new-line)
(global-set-key (kbd "C-S-L") 'move-to-window-line-top-bottom)

(global-set-key (kbd "C-S-S") 'kyt/go-to-beginning-and-search)


(provide 'init-local-simple-key-bindings)

;;; init-local-simple-key-bindings.el ends here
