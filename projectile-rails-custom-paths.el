;;; projectile-rails-remaps.el -*- lexical-binding: t; -*-

(after! projectile-rails
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

  (defun projectile-rails-find-business-or-service ()
    "Find a service."
    (interactive)
    (if (file-exists-p (concat (projectile-project-root) "app/business"))
        (projectile-rails-find-resource
           "business: "
           '(("app/business/" "\\(.+\\)\\.rb$"))
           "app/business/${filename}.rb")
      (if (file-exists-p (concat (projectile-project-root) "app/services"))
          (projectile-rails-find-resource
         "service: "
         '(("app/services/" "\\(.+\\)\\.rb$"))
         "app/services/${filename}.rb"))))

  (defun projectile-rails-find-graphql-all ()
    "Find all in graphql."
    (interactive)
    (projectile-rails-find-resource
     "graphql: "
     '(("app/graphql/" "\\(.+\\)\\.rb$"))
     "app/graphql/${filename}.rb"))

  (defun projectile-rails-find-graphql-type ()
    "Find graphql type."
    (interactive)
    (projectile-rails-find-resource
     "graphql: "
     '(("app/graphql/types/" "\\(.+\\)\\.rb$"))
     "app/graphql/types/${filename}.rb"))

  (defun projectile-rails-find-graphql-mutation ()
    "Find graphql type."
    (interactive)
    (projectile-rails-find-resource
     "graphql: "
     '(("app/graphql/mutations/" "\\(.+\\)\\.rb$"))
     "app/graphql/mutations/${filename}.rb"))

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

  (map! :leader "rs" #'projectile-rails-find-business-or-service)
  (map! :leader "rS" #'projectile-rails-find-service)
  (map! :leader "rqa" #'projectile-rails-find-graphql-all)
  (map! :leader "rqm" #'projectile-rails-find-graphql-mutation)
  (map! :leader "rqt" #'projectile-rails-find-graphql-type))
