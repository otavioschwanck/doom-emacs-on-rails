;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load (expand-file-name "modules/editor.el" doom-user-dir))
(load (expand-file-name "modules/misc.el" doom-user-dir))
(load (expand-file-name "modules/ruby.el" doom-user-dir))
(load (expand-file-name "modules/ruby-autocomplete.el" doom-user-dir))
(load (expand-file-name "modules/term.el" doom-user-dir))
(load (expand-file-name "modules/git.el" doom-user-dir))
(load (expand-file-name "modules/lsp.el" doom-user-dir))
(load (expand-file-name "modules/org.el" doom-user-dir))
(load (expand-file-name "modules/autocomplete.el" doom-user-dir))

(if (not (file-exists-p "~/.doom.d/user/config.el"))
    (progn
      (shell-command "cp ~/.doom.d/user/examples/config.el ~/.doom.d/user/config.el")
      (load (expand-file-name "user/config.el" doom-user-dir)))
  (load (expand-file-name "user/config.el" doom-user-dir)))
