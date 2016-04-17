(defun org-insert-screenshot (description)
  "Take a screenshot into a unique-named file in the current buffer file
directory and insert a link to this file."

  (defun get-data-dir ()
    (let ((dir-path (concat (file-name-directory (buffer-file-name))
                            (file-name-nondirectory (buffer-file-name))
                            ".d/")))
      (make-directory dir-path 't)
      dir-path))

  (defun get-file-name ()
    (concat (make-temp-name (get-data-dir)) ".png"))

  (defun make-screen-shot (filename)
    (start-process "shutter-screenshotting" nil
                   "shutter"
                   "-s" "-n" "-e" "-min_at_startup" "-o" filename))
  ;; 必须加 -e, 否则shutter不会产生任何event, 不会触发sentinel

  (defun insert-link (filename description)
    (if (not (string= "" description))
        (progn (insert (concat "#+CAPTION:"
                               description))
               (newline)))
    ;; 输入org link
    (insert (concat "[[" filename "]]")))

  (defun set-transparency (transparency)
    (set-frame-parameter (selected-frame) 'alpha transparency))


  (interactive (list (read-string "Description:")))

  (let ((filename (get-file-name))
        (current-frame (selected-frame)))
    (let ((current-transparency (frame-parameter current-frame
                                                 'alpha))) ;没用上
;      (set-transparency '(0 0))

      (set-process-sentinel
       (make-screen-shot filename)
       (lambda (process event)
         (set-transparency '(100 100)))))

    (insert-link filename description)))


(provide 'org-screenshot)
