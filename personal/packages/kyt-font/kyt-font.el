;;; kyt-font.el --- Simple chinese config -*- lexical-binding: t -*-

;;; Commentary:
;; 基本方针: 分别设置中英文字体时, 宽度+高度对齐很麻烦.  解决方案是直
;; 接使用一个包含中英文, 且中英混排时宽度高度对齐的字体, 如"更纱黑体"
;; (https://github.com/be5invis/Sarasa-Gothic)
;;
;; #################### 简单方案及其问题 ####################
;;
;; 使用上述字体时, 这样设置就能基本解决字体问题:
;; (set-frame-font (format "%s:pixelsize=%d" FONT-NAME PIXEL-SIZE) t t)
;; (dolist (charset '(kana han cjk-misc bopomofo gb18030 symbol))
;;   (set-fontset-font t charset FONT-NAME)) ;don't specify size here
;;
;; 配合 `default-text-scale-mode' 设置全局的字体, `text-scale-adjust'
;; 调整单个 buffer 的字体, 即可满足大多数需求.  不需要使用 `cnfonts'.
;;
;; 但是上述方案有以下问题: 使用 `default-text-scale-mode' 调整字体大小
;; 时, 在很多大小下, 中英混排宽度还是无法对齐.  核心问题是: 就算字体满
;; 足2英与1中等宽, 也需要emacs里中文字符的像素宽度为偶数, 才能保证中英
;; 混排宽度对齐.  (如果使用过程中很少调整字体大小, 也可以忽略这个问题...)
;;
;; `default-text-scale-mode', `text-scale-adjust' 调整字体大小的方法都
;; 是通过设置 default face 的 `:height' 实现的, 其单位是 1/10 磅, 难以
;; 确保对应的中文像素宽度为偶数 (满足条件的 `:height' 值间没有固定的间
;; 距; 字体不同大小下像素宽高比也不是固定的).
;;
;; (`cnfonts'给定了一个 size 列表, 调整大小时只选用这个列表中的值, 因
;; 此如果根据当前字体小心设置 size 列表, 还是可以解决这个问题的.  但是
;; 在只需要使用单一字体时, `cnfonts' 太臃肿了, 同时使用中还遇到了别的
;; 一些问题, 所以这里另起炉灶了.)
;;
;; 想要简单直接地解决这个问题, 只需要在设置字体大小时直接指明(中文)像
;; 素宽度而不是行高磅数: (set-frame-font "font-name:pixelsize=30")
;;
;; #################### 简单方案及其问题 END ####################
;;
;; `kyt-font' 的主要功能是: 1. 把单个字体(如"更纱黑体")设置为全局的默
;; 认字体 2. 方便快捷地调整全局字体大小.
;;
;; 和 `default-text-scale-mode' 的区别在于: 支持中文字体的设置; 使用像
;; 素宽度来指定字体大小.
;;
;; 如果需要调整单个 buffer 的字体大小, 可以使用 `text-scale-adjust'.
;; 使用 `kyt-font' 设置字体后, `text-scale-adjust' 可以让中英字符同时
;; 缩放, 不会出现只有英文字符缩放, 中文字符大小不变的情况(`cnfonts' 目
;; 前有这个问题).  但是, 不保证缩放后中文字符像素宽度为偶数, 因此此时
;; 中英混排可能不对齐.  因为单独调整 buffer 字体大小的情况较少, 所以还
;; 可以接受.
;;
;;; #################### How to use ####################
;;
;; (kyt-font/maybe-fallback-font)
;; (kyt-font/global-font-mode 1)
;;
;;; Code:

(require 'cl-lib)
(require 'dash)

(defvar kyt-font/font-name "Sarasa Fixed SC"
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
        ;; set font name and size in `set-frame-font'
        (set-frame-font font-name-and-size 'keep-size (list frame))
        (dolist (charset charsets)
          ;; DONT'T set size in `set-fontset-font', so that when
          ;; adjusting font size with `:height' in default face
          ;; (e.g. `text-scale-adjust'), these charsets can change
          ;; their size together with ascii chars instead of having a
          ;; fixed size.
          (set-fontset-font t charset font-name frame)
          ;; fallback: if FONT-NAME doesn't exist, search through the
          ;; existing fonts to find one that contains the glyph
          (set-fontset-font t charset (font-spec :script charset) nil 'append))))
    (unless suppress-message
      (message "%sSet Font: [ %s ]  Size: [ %d %s ]%s"
               kyt-font--message-prefix font-name font-size (if pixel "px" "pt")
               (if (display-graphic-p (selected-frame))
                   ""
                 "  (NO effect in terminal)")))))
;; (kyt-font--set-font kyt-font/font-name 24 'pixel)
;; (kyt-font--set-font kyt-font/font-name 60 'pixel)
;; (kyt-font--set-font kyt-font/font-name 16 nil)

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
  (kyt-font--set-font kyt-font/font-name
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
    (kyt-font--set-font kyt-font/font-name
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
  (kyt-font--set-font kyt-font/font-name
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
                "may not work correctly"))
        (unless (kyt-font--initialized-p)
          (kyt-font/initialize))
        (add-hook 'after-make-frame-functions 'kyt-font--set-font-for-new-frame))
    ;; Consider: when will I turn it off?  Almost only when keys are
    ;; conflicted. In this case, should not reset font here.
    (remove-hook 'after-make-frame-functions 'kyt-font--set-font-for-new-frame)
    ;; TODO when should we do this? never?
    ;; (setq kyt-font--current-font-size nil)
    ))

;;; #################### Extra Features ####################

(defvar kyt-font/fallback-fonts
  '((windows-nt . ("Microsoft YaHei" "SimSun"))
    (gnu/linux . ("Noto Sans Mono CJK SC" "Noto Sans Mono"))
    (darwin . ("Heiti SC"))
    (cygwin . ("Microsoft YaHei" "SimSun")))
  "Fallback fonts for different platforms.
An alist: key is the value of `system-type', value is a list of
font names.")

(defun kyt-font--font-exists-p (font-name)
  "Return non-nil if FONT-NAME is found.
Required to match a found font family name exactly."
  (member font-name (font-family-list)))
;; (not (not (kyt-font--font-exists-p kyt-font/font-name)))

(defun kyt-font/maybe-fallback-font ()
  "If `kyt-font/font-name' is not found, use a fallback font.
In this case, change `kyt-font/font-name' to the name of the
fallback font, which is decided based on
`kyt-font/fallback-fonts' and current `system-type'.

Note: this is not inclued in `kyt-font/global-font-mode' and need
to be called explicitly.  Should be called before
`kyt-font/initialize'."
  (unless (kyt-font--font-exists-p kyt-font/font-name)
    (if-let ((fallback (-first 'kyt-font--font-exists-p
                               (alist-get system-type kyt-font/fallback-fonts))))
        (progn
          (message "%s\"%s\" not found, set %s to \"%s\""
                   kyt-font--message-prefix kyt-font/font-name
                   "kyt-font/font-name" fallback)
          (setq kyt-font/font-name fallback))
      (message "%s\"%s\" not found, and no available fallback font. Do nothing..."
               kyt-font--message-prefix kyt-font/font-name))))
;; (kyt-font/maybe-fallback-font)

(provide 'kyt-font)

;;; kyt-font.el ends here
