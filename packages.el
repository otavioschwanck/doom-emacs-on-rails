(package! google-translate)
(package! harpoon)

(package! evil-tutor :pin "4e124cd3911dc0d1b6817ad2c9e59b4753638f28")
(package! robe :disable t)

(package! rails-routes :pin "eab995a9297ca5bd9bd4f4c2737f2fecfc36def0")
(package! rails-i18n :pin "8e87e4e48e31902b8259ded28a208c2e7efea6e9")
(package! multi-line :pin "625c608443f98bb34b4d5600d52c198509fb64d0")
(package! ruby-refactor :pin "e6b7125878a08518bffec6942df0c606f748e9ee")

(package! string-inflection :pin "fd7926ac17293e9124b31f706a4e8f38f6a9b855")

(if (file-exists-p (expand-file-name "user/packages.el" doom-user-dir))
    (load (expand-file-name "user/packages.el" doom-user-dir))
  (progn
    (shell-command "cp ~/.doom.d/user/examples/packages.el ~/.doom.d/user/packages.el")
    (load (expand-file-name "user/packages.el" doom-user-dir))))
