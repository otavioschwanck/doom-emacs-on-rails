;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load (expand-file-name "modules/editor.el" doom-private-dir))
(load (expand-file-name "modules/misc.el" doom-private-dir))
(load (expand-file-name "modules/ruby.el" doom-private-dir))
(load (expand-file-name "modules/ruby-autocomplete.el" doom-private-dir))
(load (expand-file-name "modules/term.el" doom-private-dir))
(load (expand-file-name "modules/git.el" doom-private-dir))
(load (expand-file-name "modules/lsp.el" doom-private-dir))
(load (expand-file-name "modules/org.el" doom-private-dir))
(load (expand-file-name "modules/autocomplete.el" doom-private-dir))

(if (not (file-exists-p "~/.doom.d/user/config.el"))
    (progn
      (shell-command "cp ~/.doom.d/user/examples/config.el ~/.doom.d/user/config.el")
      (load (expand-file-name "user/config.el" doom-private-dir)))
  (load (expand-file-name "user/config.el" doom-private-dir)))
