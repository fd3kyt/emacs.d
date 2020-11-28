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

(defvar kyt-font/basic-font-name "Sarasa Fixed SC"
  "The font used for both chinese and english.")

(defvar kyt-font/default-font-size 24
  "Default font size.")

(defvar kyt-font/max-font-size 100
  "Max allowed font size.  If nil, not limited.")

(defvar kyt-font/min-font-size 2
  "Min allowed font size.  If nil, use 0.")

(defvar kyt-font/use-pixel-size-p t
  "If non-nil, use pixel font size instead of point(磅) by default.")

(defvar kyt-font/chinese-charsets '(kana han cjk-misc bopomofo gb18030)
  "Chinese charsets to be set.")

(defvar kyt-font/symbol-charsets '(symbol)
  "Symbol charsets to be set.")

(defvar kyt-font--current-font-size nil
  "Current font size.")

(defvar kyt-font/font-size-step 2
  "Increment by which to adjust font size.")

(defvar kyt-font--message-prefix "[kyt-font] "
  "Prefix of messages from this package.")

(defun kyt-font--set-font (font-name font-size pixel)
  "Set font with FONT-NAME and FONT-SIZE in all (graphic) frames.
If PIXEL is non-nil, the unit of FONT-SIZE is pixel (px) instead
of point (pt)."
  (let ((font-name-and-size (format (if pixel "%s:pixelsize=%d" "%s %d")
                                    font-name font-size))
        (charsets (append kyt-font/chinese-charsets kyt-font/symbol-charsets)))
    (when (and pixel (cl-oddp font-size))
      (warn "%sUsing ODD pixel font size: %d (EVEN pixel size is recommended)"
            kyt-font--message-prefix font-size))
    (dolist (frame (frame-list))
      (when (display-graphic-p frame)
        (set-frame-font font-name-and-size 'keep-size (list frame))
        (dolist (charset charsets)
          (set-fontset-font t charset font-name frame))))
    (message "%sSet Font: [ %s ]  Size: [ %d %s ]%s"
             kyt-font--message-prefix font-name font-size (if pixel "px" "pt")
             (if (display-graphic-p)
                 ""
               "  (NO effect in terminal)"))))
;; (kyt-font--set-font kyt-font/basic-font-name 24 'pixel)
;; (kyt-font--set-font kyt-font/basic-font-name 60 'pixel)
;; (kyt-font--set-font kyt-font/basic-font-name 16 nil)

(defun kyt-font/initialize-font ()
  "Initialize font based on `kyt-font' variables."
  (interactive)
  (kyt-font--set-font kyt-font/basic-font-name
                      kyt-font/default-font-size
                      kyt-font/use-pixel-size-p)
  (setq kyt-font--current-font-size kyt-font/default-font-size))

(defun kyt-font--adjust-font-size (delta)
  "Adjust font size with DELTA."
  (unless kyt-font--current-font-size
    (error "%sAdjust font size before initialized" kyt-font--message-prefix))
  (let ((new-size (min (or kyt-font/max-font-size most-positive-fixnum)
                       (max (or kyt-font/min-font-size 0)
                            (+ delta kyt-font--current-font-size)))))
    (setq kyt-font--current-font-size new-size)
    (kyt-font--set-font kyt-font/basic-font-name
                        new-size
                        kyt-font/use-pixel-size-p)))

(defun kyt-font/increase-font-size (steps)
  "Increase font size with STEPS."
  (interactive "p")
  (kyt-font--adjust-font-size (* steps kyt-font/font-size-step)))

(defun kyt-font/decrease-font-size (steps)
  "Decrease font size with STEPS."
  (interactive "p")
  (kyt-font--adjust-font-size (- (* steps kyt-font/font-size-step))))

;; (defun kyt/set-font-size (size)
;;   "Set font size to SIZE."
;;   (interactive "nFont size? ")
;;   (let ((font-string (format "%s %d" kyt-font/basic-font-name size)))
;;     (set-frame-font font-string nil t)
;;     (set-fontset-font t 'han font-string)))

;;; if you want to adjust font size based on screen resolution:
;;; (if (> (x-display-pixel-width) 1600) (use-big-size) (use-small-size))

(define-minor-mode kyt-font/global-set-font-mode
  "Set font in every frame."
  :global t
  :require 'kyt-font
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-M-=") 'kyt-font/increase-font-size)
            (define-key map (kbd "C-M--") 'kyt-font/decrease-font-size)
            (define-key map (kbd "C-M-0") 'kyt-font/initialize-font)
            map)
  (if kyt-font/global-set-font-mode
      ;; TODO
      (add-hook 'after-make-frame-functions #'default-text-scale--update-for-new-frame)
    (remove-hook 'after-make-frame-functions #'default-text-scale--update-for-new-frame)))

(defun get-default-face-height-and-pixel-size ()
  "Same as name."
  (interactive)
  (message "%s"
           (list :point-height (face-attribute 'default :height)
                 :pixel-width (window-font-width)
                 :pixel-height (window-font-height))))

(defun set-default-face-height (height)
  "Set the HEIGHT of default face.
Unit of height: 1/10 point."
  (face-spec-set 'default `((t (:height ,height)))))
;; (set-default-face-height 160)

(defun get-chinese-pixel-width ()
  "As name."
  (save-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (insert "中")
      (aref (aref (font-get-glyphs (font-at 1) 1 2) 0) 4))))
;; (get-chinese-pixel-width)

(defun test-face-height-with-even-pixel-width (start end)
  "Find all height values between START, END with even pixel width."
  (let ((previous-height (face-attribute 'default :height)))
    (unwind-protect
        (-filter (lambda (arg) (and (cl-evenp (nth 3 arg))
                               (cl-evenp (nth 3 arg))))
                 (mapcar (lambda (height)
                           (set-default-face-height height)
                           (list (face-attribute 'default :height)
                                 (frame-char-width)
                                 (frame-char-height)
                                 (get-chinese-pixel-width)))
                         (number-sequence start end 5)))
      (set-default-face-height previous-height))))
;; (test-face-height-with-even-pixel-width 5 200)

(provide 'kyt-font)

;;; kyt-font.el ends here
