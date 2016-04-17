;; config for publish site from org files
(require 'org-publish)

(setq org-publish-project-alist
      '(
        ;; These are the main web files
        ("org-notes"
         :base-directory "~/publish/org/" ;; Change this to your local dir
         :base-extension "org"
         :publishing-directory "~/publish/output"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble nil
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         :sitemap-title "sitemap"
         :section-numbers nil
         :table-of-contents t
         :style "<link rel='stylesheet' type='text/css' href='css/org-manual.css' />"
         :style-include-default nil
         )

        ;; These are static files (images, pdf, etc)
        ("org-static"
         :base-directory "~/publish/org/" ;; Change this to your local dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|txt\\|asc"
         :publishing-directory "~/publish/output/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("org" :components ("org-notes" "org-static"))
        )
      )

(defun myweb-publish nil
  "Publish myweb."
  (interactive)
  (org-publish-all))

(myweb-publish)
