;; (custom-set-variables `(verilog-tool 'verilog-compiler')
;; (custom-set-variables `(verilog-linter "vlint ..."')
;; (custom-set-variables `(verilog-coverage "coverage ..."')
;; (custom-set-variables `(verilog-simulator "verilator ... "')
;; (custom-set-variables `(verilog-compiler "(make -f makev :__FILE__)"' )

(require 'verilog-mode)
;;避免变量未定义

(require 'vcpu-project)

(provide 'init-local-verilog)
