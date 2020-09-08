;;; ruby.el --- Ruby config                          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Doe

;; Author: Otávio <otavioschwanck@gmail.com>
;; Keywords: ruby

;; Better C-j and C-k
(map! :map ruby-mode-map
      "C-k" #'ruby-beginning-of-block
      "C-j" #'ruby-end-of-block)

;; Rubocop com C-=
(after! ruby-mode
  (defun msc/revert-buffer-noconfirm ()
    "Call `revert-buffer' with the NOCONFIRM argument set."
    (interactive)
    (revert-buffer nil t))

  (defun rubocop-on-current-file ()
    "RUBOCOP ON CURRENT_FILE."
    (interactive)
    (save-buffer)
    (message "%s" (shell-command-to-string
                   (concat "bundle exec rubocop -a "
                           (shell-quote-argument (buffer-file-name)))))
    (msc/revert-buffer-noconfirm))
  (map! :mode ruby-mode-map :leader "=" #'rubocop-on-current-file))

;; Projectile globally with SPC r
(require 'projectile-rails)
(map! :leader "r" #'projectile-rails-command-map)

;; Fix projectile texts
(after! which-key
  (push '((nil . "projectile-rails-\\(.+\\)") . (nil . "\\1"))
        which-key-replacement-alist))

(after! ruby-mode
  ;; SPC m C to copy class name, super useful to test things on console.
  (defun endless/-ruby-symbol-at-point ()
    (let ((l (point)))
      (save-excursion
        (forward-sexp 1)
        (buffer-substring l (point)))))

  (defun endless/ruby-copy-class-name ()
    (interactive)
    (save-excursion
      (let ((name nil)
            (case-fold-search nil))
        (skip-chars-backward (rx (syntax symbol)))
        (when (looking-at-p "\\_<[A-Z]")
          (setq name (endless/-ruby-symbol-at-point)))
        (while (ignore-errors (backward-up-list) t)
          (when (looking-at-p "class\\|module")
            (save-excursion
              (forward-word 1)
              (skip-chars-forward "\r\n[:blank:]")
              (setq name (if name
                             (concat (endless/-ruby-symbol-at-point) "::" name)
                           (endless/-ruby-symbol-at-point))))))
        (kill-new name)
        (message "Copied %s" name))))

  ;; binding it to SPC m C
  (map! :map ruby-mode-map :localleader "C" #'endless/ruby-copy-class-name)
  )

(remove-hook 'text-mode-hook #'visual-line-mode)

(defun file-path-to-test (filename)
  (if (string-match-p "/spec/" filename)
      (if (string-match-p "/admin/" filename)
          (concat
           (replace-regexp-in-string "/spec/controllers/" "/app/" (file-name-directory filename))
           (singularize-string (replace-regexp-in-string "_controller_spec" "" (file-name-base filename)))
           "."
           (file-name-extension filename))
        (concat
         (replace-regexp-in-string "/spec/" "/app/" (file-name-directory filename))
         (replace-regexp-in-string "_spec" "" (file-name-base filename))
         "."
         (file-name-extension filename)))
    (if (string-match-p "/admin/" filename)
        (concat
         (replace-regexp-in-string "/app/" "/spec/controllers/" (file-name-directory filename))
         (pluralize-string (file-name-base filename))
         "_controller_spec."
         (file-name-extension filename))
      (concat
       (replace-regexp-in-string "/app/" "/spec/" (file-name-directory filename))
       (file-name-base filename)
       "_spec."
       (file-name-extension filename)))))

(defun goto-test-and-vsplit ()
  (interactive)
  (if (string-match-p "/spec/" buffer-file-name) (find-file (file-path-to-test buffer-file-name)))
  (delete-other-windows)
  (evil-window-vsplit)
  (evil-window-right 1)
  (if (string-match-p "/app/" buffer-file-name) (find-file (file-path-to-test buffer-file-name))))

(defun goto-test ()
  (interactive)
  (find-file (file-path-to-test buffer-file-name)))

(map! :mode ruby-mode-map :leader "a" 'goto-test)
(map! :mode ruby-mode-map :leader "A" 'goto-test-and-vsplit)

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(ruby-reek)))
