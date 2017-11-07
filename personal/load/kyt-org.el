;;; kyt-org.el --- Summary

;;; Commentary:
;;

;;; Code:


(defun kyt/list-all-heading ()
  (let ((tree (org-element-parse-buffer)))
    (org-element-map tree 'headline
      (lambda (hl) (substring-no-properties
               (car (org-element-property :title hl)))))))


(defun kyt/test-org-element-tag ()
  (interactive)
  (let ((tree (org-element-parse-buffer)))
    (mapc 'print
          (org-element-map tree 'headline
            (lambda (hl) (when (> (length (org-element-property :tags hl)) 1)
                      (substring-no-properties
                       (car (org-element-property :title hl)))))))))


;; go to :begin and org-set-property doesn't work. org-set-property
;; will add new lines. the data of point is out of date.
(defun kyt/test-org-element-add-property ()
  (interactive)
  (let* ((tree (org-element-parse-buffer))
         (headlines (org-element-map tree 'headline
                      (lambda (hl) (when (> (length (org-element-property :tags hl)) 0)
                                hl))))
         (markers (mapcar (lambda (hl) (progn
                                    (goto-char (org-element-property :begin hl))
                                    (point-marker)))
                          headlines)))
    (cl-loop for m in markers
             do (progn
                  (goto-char m)
                  (org-set-property "id" "100")))))


(org-sparse-tree)

(provide 'kyt-org)

;;; kyt-org.el ends here
