;; Please dont modify this file.  Edit ~/.doom.d/user-packages.el instead.

(package! string-inflection)
(package! kubernetes)
(package! kubernetes-evil)
(package! multi-line)
(package! ruby-refactor)
(package! command-log-mode)
(package! google-translate)
(package! ruby-hash-syntax)
(package! f)
(package! lsp-tailwindcss)
(package! crdt)
(package! rails-routes)
(package! rails-i18n)
(package! harpoon)
(package! ruby-json-to-hash)
(unpin! smartparens)
(package! dogears)
(package! evil-tutor)

(if (file-exists-p (expand-file-name "user-packages.el" doom-private-dir))
    (load (expand-file-name "user-packages.el" doom-private-dir))
  (progn
    (shell-command "cp ~/.doom.d/user-packages.example.el ~/.doom.d/user-packages.el")
    (load (expand-file-name "user-packages.el" doom-private-dir))))
