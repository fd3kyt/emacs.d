;;; init-local-langtool.el --- Summary -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:


(declare-function require-package "init-elpa")
(require-package 'langtool)
(require 'langtool)
(setq langtool-language-tool-jar
      "/home/fd3kyt/local/LanguageTool-4.1/languagetool-commandline.jar")
(setq langtool-language-tool-server-jar
      "/home/fd3kyt/local/LanguageTool-4.1/languagetool-server.jar")

(setq langtool-default-language "en-US")
;; (setq langtool-java-bin "/path/to/java")

(provide 'init-local-langtool)

;;; init-local-langtool.el ends here
