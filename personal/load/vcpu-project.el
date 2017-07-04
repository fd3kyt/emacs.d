(require 'xah-replace-pairs)

(defvar vcpu-dir "~/Project/verilog/single")

(defun vcpu-replace-op-to-binary (φbegin φend)
  "Replace alpha to α etc in current line or selection.
Version 2015-04-28"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (xah-replace-regexp-pairs-region
   φbegin
   φend
   '(
     ["\\band\\b" "000000"]
     ["\\bandi\\b" "000001"]
     ["\\bor\\b" "000010"]
     ["\\bori\\b" "000011"]
     ["\\badd\\b" "000100"]
     ["\\baddi\\b" "000101"]
     ["\\bsub\\b" "000110"]
     ["\\bsubi\\b" "000111"]
     ["\\bload\\b" "001000"]
     ["\\bstore\\b" "001001"]
     ["\\bbeq\\b" "001010"]
     ["\\bbne\\b" "001011"]
     ["\\bbranch\\b" "001100"]
     ["\\bsll\\b" "001101"]
     ["\\bsrl\\b" "001110"]
     ["\\bsra\\b" "001111"])))


(defun verilog-vcpu-set-compile-command ()
  (interactive)
  (set (make-local-variable 'compile-command)
       (concat "make "
               (file-name-base buffer-file-name)))
  (verilog-modify-compile-command))

(defun verilog-vcpu-set-command-auto-save-compile ()
  (interactive)
  (verilog-vcpu-set-compile-command)
  (verilog-auto-save-compile))

(defvar verilog-mode-hook 'verilog-vcpu-set-compile-command)

(defun verilog-cpuproject-open-gtkwave ()
  (interactive)
  (shell-command (concat "jumpapp gtkwave "
                         vcpu-dir
                         "/vcd/"
                         (file-name-base buffer-file-name)
                         ".vcd")))


(define-key verilog-mode-map
  (kbd "C-c s") 'verilog-cpuproject-open-gtkwave)

(define-key verilog-mode-map
  (kbd "C-c C-s") 'verilog-vcpu-set-command-auto-save-compile)



(provide 'vcpu-project)
