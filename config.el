;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Otávio Schwanck dos Santos"
      user-mail-address "otavioschwanck@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Fira Code" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(after! deft
  (setq deft-extensions '("org" "txt" ""))
  (setq deft-recursive t))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(defvar robe-time-to-start 20
  "Set the time to start robe after starting inf-ruby-console-auto")

(defvar start-rails-server nil)

(defun open-rails-project (&optional DIRECTORY CACHE)
  (interactive)
  (magit-status DIRECTORY)
  (if (fboundp 'magit-fetch-from-upstream)
      (call-interactively #'magit-fetch-from-upstream)
    (call-interactively #'magit-fetch-current))
  (when (file-exists-p (concat DIRECTORY "Gemfile"))
    (message "Gemfile found on the project.  Starting rails console in the background.  Nice code to you!")
    (when start-rails-server
      (progn
        (projectile-rails-console nil)
        (+popup/close-all)))))

(setq +workspaces-switch-project-function #'open-rails-project)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(load (expand-file-name "rails-routes.el" doom-private-dir))

(load (expand-file-name "library_fixes.el" doom-private-dir))
(load (expand-file-name "editor.el" doom-private-dir))
(load (expand-file-name "ruby.el" doom-private-dir))
(load (expand-file-name "company.el" doom-private-dir))
(load (expand-file-name "performance.el" doom-private-dir))
(load (expand-file-name "projectile-rails-custom-finders.el" doom-private-dir))

(when (file-exists-p (expand-file-name "user.el" doom-private-dir))
  (load (expand-file-name "user.el" doom-private-dir)))
