;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Please dont modify this file.  Edit ~/.doom.d/user-settings.el instead.

(if (not (file-exists-p "~/.doom.d/doom-settings.el"))
    (org-babel-load-file
     (expand-file-name "doom-settings.org" doom-private-dir))
  (load (expand-file-name "doom-settings.el" doom-private-dir)))
