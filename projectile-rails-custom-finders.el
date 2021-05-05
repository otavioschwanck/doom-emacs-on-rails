;;; projectile-rails-remaps.el -*- lexical-binding: t; -*-

(after! projectile-rails
  (defun projectile-rails-find-admin ()
    "Find a model."
    (interactive)
    (projectile-rails-find-resource
     "admin: "
     '(("app/admin/" "\\(.+\\)\\.rb$"))
     "app/admin/${filename}.rb"))

  (defun projectile-rails-find-current-admin ()
    "Find a model for the current resource."
    (interactive)
    (projectile-rails-find-current-resource "app/admin/"
                                            "${singular}\\.rb$"
                                            'projectile-rails-find-admin))

  (defun projectile-rails-find-service ()
    "Find a service."
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
           "app/business/${filename}.rb"))))

  (defun projectile-rails-find-graphql-all ()
    "Find all in graphql."
    (interactive)
    (projectile-rails-find-resource
     "graphql: "
     '(("app/graphql/" "\\(.+\\)\\.rb$"))
     "app/graphql/${filename}.rb"))

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
        (message "Service or business folder not found"))))

  (map! :leader "rd" #'otavio/go-to-latest-migration)
  (map! :leader "rt" #'projectile-rails-find-admin)
  (map! :leader "rT" #'projectile-rails-find-current-admin)
  (map! :leader "rs" #'projectile-rails-find-service)
  (map! :leader "rS" #'projectile-rails-find-current-service)
  (map! :leader "rq" #'projectile-rails-find-graphql-all))
