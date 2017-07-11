;; (setq verilog-tool 'verilog-compiler')
;; (setq verilog-linter "vlint ...")
;; (setq verilog-coverage "coverage ...")
;; (setq verilog-simulator "verilator ... ")
;; (setq verilog-compiler "(make -f makev :__FILE__)")

(require-package 'verilog-mode)
;;避免变量未定义

(require 'vcpu-project)

(provide 'init-local-verilog)
