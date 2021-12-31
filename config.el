;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(if (not (file-exists-p "~/.doom.d/doom-settings.el"))
    (org-babel-load-file
     (expand-file-name "doom-settings.org" doom-private-dir))
  (load (expand-file-name "doom-settings.el" doom-private-dir)))
