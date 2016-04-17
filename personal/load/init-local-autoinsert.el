;;; init-local-autoinsert.org --- templates for auto-insert

;;; Commentary:
;;


;;; Code:
(require 'autoinsert)
;; c
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.c\\'" . "C skeleton")
     '(
       "Short description: "
       "/**\n * "
       (file-name-nondirectory (buffer-file-name))
       " -- " str \n
       " *" \n
       " * Written on " (format-time-string "%A, %e %B %Y.") \n
       " */" > \n \n
       "#include <stdio.h>" \n
       "#include \""
       (file-name-sans-extension
        (file-name-nondirectory (buffer-file-name)))
       ".h\"" \n \n
       "int main()" \n
       "{" > \n
       > _ \n
       "}" > \n)))


;; c++
(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.cpp\\'" . "C++ skeleton")
     '("Short description: "
       "/*" \n
       (file-name-nondirectory (buffer-file-name))
       " -- " str \n
       " */" > \n \n
       "#include <iostream>" \n \n
       "using namespace std;" \n \n
       "main()" \n
       "{" \n
       > _ \n
       "}" > \n)))


(provide 'init-local-autoinsert)
;;; init-local-autoinsert.el ends here
