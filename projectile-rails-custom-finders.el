;;; projectile-rails-remaps.el -*- lexical-binding: t; -*-

(after! projectile-rails
  (defun projectile-rails-find-service ()
    "Find a model."
    (interactive)
    (if (file-exists-p (concat (projectile-project-root) "app/services"))
        (projectile-rails-find-resource
         "service: "
         '(("app/services/" "\\(.+\\)\\.rb$"))
         "app/services/${filename}.rb")
      (if (file-exists-p (concat (projectile-project-root) "app/business"))
          (projectile-rails-find-resource
           "service: "
           '(("app/business/" "\\(.+\\)\\.rb$"))
           "app/business/${filename}.rb")))

  (defun projectile-rails-find-current-service ()
    "Find a model for the current resource."
    (interactive)
    (if (file-exists-p (concat (projectile-project-root) "app/services"))
        (projectile-rails-find-current-resource "app/services/"
                                                "${singular}\\.rb$"
                                                'projectile-rails-find-service)
      (if (file-exists-p (concat (projectile-project-root) "app/business"))
          (projectile-rails-find-current-resource "app/business/"
                                                  "${singular}\\.rb$"
                                                  'projectile-rails-find-service)
        (message "Service or business folder not found")))))

  (map! :leader "rs" #'projectile-rails-find-service)
  (map! :leader "rS" #'projectile-rails-find-current-service))
