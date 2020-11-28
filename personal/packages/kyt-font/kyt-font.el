;;; kyt-font.el --- Simple chinese config -*- lexical-binding: t -*-

;;; Commentary:
;; 基本思路: 分别设置中英文字体时, 宽度+高度对齐很麻烦.  直接使用包含
;; 中英文, 且中英混排时宽度高度对齐的字体, 例如"更纱黑体"
;; (https://github.com/be5invis/Sarasa-Gothic)

;; 注意: 就算字体保证2英与1中等宽, 也需要emacs里中文字符的像素宽度为偶
;; 数, 才能保证中英混排宽度对齐

;; ;;; set english chars
;; (set-frame-font "Sarasa Fixed SC:pixelsize=16" nil t)
;; ;;; set chinese chars
;; (set-fontset-font t 'han "Sarasa Fixed SC")

;;; Code:

(require 'cl-lib)
(require 'dash)

(defvar kyt-font/basic-font-name "Sarasa Fixed SC"
  "The font used for both chinese and english.")

(defvar kyt-font/use-pixel-size-p t
  "If non-nil, use pixel font size instead of point(磅) by default.")
(defvar kyt-font/initial-font-size 24
  "Initial font size when initialized.")
(defvar kyt-font/max-font-size 100
  "Max allowed font size.  If nil, not limited.")
(defvar kyt-font/min-font-size 2
  "Min allowed font size.  If nil, use 0.")
(defvar kyt-font/font-size-step 2
  "Increment by which to adjust font size.")

(defvar kyt-font/chinese-charsets '(kana han cjk-misc bopomofo gb18030)
  "Chinese charsets to be set.")
(defvar kyt-font/symbol-charsets '(symbol)
  "Symbol charsets to be set.")

(defvar kyt-font--current-font-size nil
  "Current font size.")
(defvar kyt-font--message-prefix "[kyt-font] "
  "Prefix of messages from this package.")

(defun kyt-font--set-font (font-name font-size pixel
                                     &optional frames suppress-message)
  "Set font with FONT-NAME and FONT-SIZE in all (graphic) frames.
If PIXEL is non-nil, the unit of FONT-SIZE is pixel (px) instead
of point (pt).

If FRAMES is not nil, set only frames in this list instead of all
frames.

If SUPPRESS-MESSAGE is non-nil, don't show the success message."
  (let ((font-name-and-size (format (if pixel "%s:pixelsize=%d" "%s %d")
                                    font-name font-size))
        (charsets (append kyt-font/chinese-charsets kyt-font/symbol-charsets)))
    (when (and pixel (cl-oddp font-size))
      (warn "%sUsing ODD pixel font size: %d (EVEN pixel size is recommended)"
            kyt-font--message-prefix font-size))
    (dolist (frame (or frames (frame-list)))
      (when (display-graphic-p frame)
        (set-frame-font font-name-and-size 'keep-size (list frame))
        (dolist (charset charsets)
          (set-fontset-font t charset font-name frame))))
    (unless suppress-message
      (message "%sSet Font: [ %s ]  Size: [ %d %s ]%s"
               kyt-font--message-prefix font-name font-size (if pixel "px" "pt")
               (if (display-graphic-p)
                   ""
                 "  (NO effect in terminal)")))))
;; (kyt-font--set-font kyt-font/basic-font-name 24 'pixel)
;; (kyt-font--set-font kyt-font/basic-font-name 60 'pixel)
;; (kyt-font--set-font kyt-font/basic-font-name 16 nil)

(defun kyt-font--initialized-p ()
  "Return non-nil if `kyt-font' is initialzed."
  kyt-font--current-font-size)

(defun kyt-font/initialize (&optional save-current)
  "Initialize font based on `kyt-font' variables.
With prefix argument SAVE-CURRENT, save current `kyt-font' font
size as initial font size afterwards."
  (interactive "P")
  (when save-current
    (cl-assert (kyt-font--initialized-p))
    (cl-assert (numberp kyt-font--current-font-size))
    (setq kyt-font/initial-font-size kyt-font--current-font-size))
  (kyt-font--set-font kyt-font/basic-font-name
                      kyt-font/initial-font-size
                      kyt-font/use-pixel-size-p)
  (setq kyt-font--current-font-size kyt-font/initial-font-size))

(defun kyt-font--adjust-font-size (delta)
  "Adjust font size with DELTA."
  (unless (kyt-font--initialized-p)
    (error "%sAdjust font size before initialized" kyt-font--message-prefix))
  (let ((new-size (min (or kyt-font/max-font-size most-positive-fixnum)
                       (max (or kyt-font/min-font-size 0)
                            (+ delta kyt-font--current-font-size)))))
    (kyt-font--set-font kyt-font/basic-font-name
                        new-size
                        kyt-font/use-pixel-size-p)
    (setq kyt-font--current-font-size new-size)))

(defun kyt-font/increase-font-size (steps)
  "Increase font size with STEPS."
  (interactive "p")
  (kyt-font--adjust-font-size (* steps kyt-font/font-size-step)))

(defun kyt-font/decrease-font-size (steps)
  "Decrease font size with STEPS."
  (interactive "p")
  (kyt-font--adjust-font-size (- (* steps kyt-font/font-size-step))))

;;; if you want to adjust font size based on screen resolution:
;; (if (> (display-pixel-width) 1600) (use-big-size) (use-small-size))
;;; `display-pixel-width', `display-mm-width'

(defun kyt-font--set-font-for-new-frame (frame)
  "Set font in new FRAME."
  (kyt-font--set-font kyt-font/basic-font-name
                      kyt-font--current-font-size
                      kyt-font/use-pixel-size-p
                      (list frame) 'suppress-message))

(defvar default-text-scale-mode)        ;for warning
(define-minor-mode kyt-font/global-font-mode
  "Set font in every frame."
  :global t
  :require 'kyt-font
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-M-=") 'kyt-font/increase-font-size)
            (define-key map (kbd "C-M--") 'kyt-font/decrease-font-size)
            (define-key map (kbd "C-M-0") 'kyt-font/initialize)
            map)
  (if kyt-font/global-font-mode
      (progn
        (when default-text-scale-mode
          (warn "%s%s%s"
                kyt-font--message-prefix
                "Using together with purcell's default-text-scale-mode, "
                ;; e.g. font size of new frames may not be set
                ;; correctly
                "may not work correctly."))
        (unless (kyt-font--initialized-p)
          (kyt-font/initialize))
        (add-hook 'after-make-frame-functions 'kyt-font--set-font-for-new-frame))
    ;; Consider: when will I turn it off?  Almost only when keys are
    ;; conflicted. In this case, should not reset font here.
    (remove-hook 'after-make-frame-functions 'kyt-font--set-font-for-new-frame)
    ;; TODO when should we do this? never?
    ;; (setq kyt-font--current-font-size nil)
    ))

(provide 'kyt-font)

;;; kyt-font.el ends here
