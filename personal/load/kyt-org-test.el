;;; kyt-org-test.el --- test for kyt-org

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

;; Use markers, ok now.
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
    (dolist (m markers)
      (goto-char m)
      (org-set-property "id" "234"))))


(defun kyt/test-extract-fields ()
  (interactive)
  (let* ((tree (org-element-parse-buffer))
         (headlines (org-element-map tree 'headline
                      (lambda (hl) (when (member "basic@anki"
                                             (org-element-property :tags hl))
                                 hl))
                      nil nil 'headline)))
    (with-output-to-temp-buffer "*parsed*"
      (dolist (hl headlines)
        (pp hl)))))


;; no, headline is an element, not an object
(org-element-parse-secondary-string
 (buffer-substring 343 386) (list 'headline))

;; yes, this can parse elements
(org-element--parse-elements
 343 386
 'first-section nil nil nil (list 'org-data nil))


(defun kyt/test-org-element-tag ()
  (interactive)
  (with-output-to-temp-buffer "parsed-tree"
    (pp (let ((tree (org-element-parse-buffer)))
          (org-element-map tree 'headline
            (lambda (hl) (when (member "basic@anki" (org-element-property :tags hl))
                       (org-element-contents hl))))))))


(defun kyt/test-extract-definition-list ()
  (interactive)
  (let* ((data (let ((tree (org-element-parse-buffer)))
                 (append
                  (org-element-map tree 'headline
                    (lambda (hl) (when (member "basic@anki" (org-element-property :tags hl))
                               (org-element-contents hl)))))))
         (objects (org-element-map data
                      'item
                    (lambda (item)
                      (mapcar 'substring-no-properties
                              (list (car
                                     (org-element-property :tag item))
                                    (car (org-element-contents
                                          (car (org-element-contents item)))))))
                    nil nil 'headline)))
    (with-output-to-temp-buffer "*parsed*"
      (dolist (o objects)
        (pp o)))))


(provide 'kyt-org-test)

;;; kyt-org-test.el ends here
