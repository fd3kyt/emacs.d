;;; koe-process.el --- Summary

;;; Commentary:
;;

;;; Code:

(let* ((buffer (get-buffer-create "*echo*"))
       (process (start-process "*echo*" buffer "echo")))
  (process-send-string process "Hello world!")
  (switch-to-buffer buffer))


(provide 'koe-process)

;;; koe-process.el ends here
