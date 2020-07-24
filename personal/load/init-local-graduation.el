;;; init-local-graduation.el --- Summary -*- lexical-binding: t -*-

;;; Commentary:
;; temp code for my graduation design

;;; Code:

(defun kg/format-pseudo-code ()
  "Format marked cpp code block for visio pseudo code.

Work in place"
  (interactive)
  (save-restriction
    (narrow-to-region (region-beginning) (region-end))
    (deactivate-mark)
    (goto-char (point-min))
    ;; (replace-regexp "^#\\+BEGIN_SRC.*$" "#+BEGIN_VERSE")
    (re-search-forward "^#\\+BEGIN_SRC.*$")
    (replace-match "#+BEGIN_VERSE")
    ;; (replace-regexp "^#\\+END_SRC" "#+END_VERSE")
    (re-search-forward "^#\\+END_SRC")
    (replace-match "#+END_VERSE")
    (goto-char (point-min))
    (let (start-line end-line start-pos)
      ;; get line numbers of start and end
      (search-forward "#+BEGIN_VERSE")
      (setq start-line (1+ (line-number-at-pos (match-end 0))))
      (search-forward "#+END_VERSE")
      (setq end-line (1- (line-number-at-pos (match-end 0))))
      ;; insert line number for pseudo code
      (goto-char (point-min))
      (forward-line (1- start-line))
      (beginning-of-line)
      (setq start-pos (point))
      (dotimes (i (1+ (- end-line start-line)))
        (beginning-of-line)
        (insert (format "%2d." (1+ i)))
        (forward-line))
      (beginning-of-line)
      ;; TODO region not activated?
      (push-mark start-pos t t)
      )))

(defun kg/handle-punctuations-and-space ()
  "Replace english punctuations to chinese punctuations.

Remove useless spaces.

Work on whole buffer."
  (interactive)

  (goto-char (point-min))
  (replace-regexp "\\(\\cc\\)," "\\1，")

  (goto-char (point-min))
  (replace-regexp ", ?\\(\\cc\\)" "，\\1")

  (goto-char (point-min))
  (replace-regexp "\\(\\cc\\)\\." "\\1。")

  (goto-char (point-min))
  (replace-regexp "\\. ?\\(\\cc\\)" "。\\1")

  (goto-char (point-min))
  (replace-regexp "\\(\\cc\\):" "\\1：")

  (goto-char (point-min))
  (replace-regexp ": ?\\(\\cc\\)" "：\\1")

  (goto-char (point-min))
  (replace-regexp "\\([，。：]\\) " "\\1")

  (goto-char (point-min))
  (replace-regexp "\\(\\cc\\) " "\\1")

  (goto-char (point-min))
  (replace-regexp "\\([^ ]\\) \\(\\cc\\)" "\\1\\2")

  (goto-char (point-min))
  (replace-regexp "\\\\item\\(\\cc\\)" "\\\\item \\1")

  (message nil)
  )

(provide 'init-local-graduation)

;;; init-local-graduation.el ends here
