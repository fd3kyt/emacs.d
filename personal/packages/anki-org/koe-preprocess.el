;;; koe-preprocess.el --- Preprocess for anki

;;; Commentary:
;;

;;; Code:

(require 'koe-loop)

;; Problem: id field name. org-id.el uses "ID", but what if ID already
;; exists? Check id format first, Configurable: raise error, prompt,
;; replace, keep.



(provide 'koe-preprocess)

;;; koe-preprocess.el ends here
