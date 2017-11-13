;;; koe-loop.el --- Loop on org-element data

;;; Commentary:
;;

;;; Code:

(require 'org-element)

(defun --point-to-marker (point)
  "Return value of POINT, as a marker object."
  (let ((marker (make-marker)))
    (set-marker marker point)
    marker))


;; should not pass the target to fun, because the positions in the
;; parse trees is out of date.
(defun koe-loop-on-element-list (list fun)
  "LIST: output of `org-element-map', `identity'.
FUN: no arguments, call at the beginning of each element of
LIST."
  (save-excursion
    (let ((markers (mapcar (lambda (target)
                             (--point-to-marker
                              (org-element-property :begin target)))
                           list)))
      (dolist (marker markers)
        (goto-char marker)
        (apply fun ())))))

;; (koe-loop-on-element-list (org-element-map
;;                               (org-element-parse-buffer)
;;                               'headline 'identity)
;;                           (lambda () (org-set-property "id" "2345")))

(defmacro koe-do-element-list (list &rest body)
  "Go to the beginning of each element in LIST, run BODY.
Modification is allowed in BODY because we use markers to store
the position."
  (let ((markers-symbol (make-symbol "markers"))
        (marker-symbol (make-symbol "marker")))
    `(save-excursion
       (let ((,markers-symbol
              (mapcar (lambda (target)
                        (--point-to-marker
                         (org-element-property :begin target)))
                      ,list)))
         (dolist (,marker-symbol ,markers-symbol)
           (goto-char ,marker-symbol)
           ,@body)))))

;; (koe-do-element-list
;;  (org-element-map
;;      (org-element-parse-buffer)
;;      'headline 'identity)
;;  (org-set-property "id" "2345"))


(provide 'koe-loop)

;;; koe-loop.el ends here
