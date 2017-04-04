;;; kyt-lib.el --- Util for kyt

;;; Commentary:
;;

;;; Code:

(defun kyt/get-reasonable-file-name (string)
  "Replace special characters in STRING, so that it can be used
as a file name."
  (replace-regexp-in-string "[: *&^%$#@!?\"'/]" "_"
                            string))


(provide 'kyt-lib)

;;; kyt-lib.el ends here
