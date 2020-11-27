;;; ruby.el --- Ruby config                          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Doe

;; Author: Otávio <otavioschwanck@gmail.com>
;; Keywords: ruby

;; Better C-j and C-k
(map! :map ruby-mode-map
      "C-k" #'ruby-beginning-of-block
      "C-j" #'ruby-end-of-block)

(setq split-ruby-giant-string-default 125)

(defun otavio/split-ruby-giant-string (&optional line-split-real)
  (interactive)
  (if (not line-split-real)
      (setq line-split-real (read-number "split in column:" split-ruby-giant-string-default)))
  (setq line-split (- line-split-real 3))
  (move-to-column line-split)
  (setq char-at-point-is-closing (eq ?\" (char-after)))
  (if (not char-at-point-is-closing)
      (if (eq (current-column) line-split)
          (progn
            ;; Start refactoring
            (if (< (+ (current-indentation) 5 (length (word-at-point))) line-split)
                (backward-word))
            (insert "\"\"")
            (backward-char)
            (newline-and-indent)
            (forward-line -1)
            (end-of-line)
            (insert " \\")
            (forward-line 1)
            (indent-according-to-mode)
            (end-of-line)
            (if (> (current-column) line-split-real)
                (otavio/split-ruby-giant-string line-split-real)
                )
            )
          )))

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
  (setq-default flycheck-disabled-checkers '(ruby-reek lsp)))

(defun otavio/chomp (str)
  "Trim leading and trailing whitespace from STR."
  (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)" "" str))

(defun otavio/delete-current-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(defun otavio/grb ()
  (interactive)
  (setq line-text (buffer-substring (line-beginning-position) (line-end-position)))
  (setq splitted-string (s-split ";" line-text))
  (delete-region (line-beginning-position) (line-end-position))
  (dolist (item splitted-string)
    (setq splitted-item (s-split "\\@" (otavio/chomp item)))
    (setq method-name (nth 0 splitted-item))
    (if (equal method-name "init")
        (setq method-name "initialize"))
    (insert (concat "def " method-name))
    (if (eq (length splitted-item) 2)
        (progn
          (insert "(")
          (dolist (arg (s-split "," (nth 1 splitted-item)))
            (insert (concat arg ", ")))
          (delete-char -2)
          (insert ")")))
    (indent-region (line-beginning-position) (line-end-position))
    (newline)
    (if (eq (length splitted-item) 2)
        (if (equal (nth 0 splitted-item) "init")
            (progn
              (dolist (arg (s-split "," (nth 1 splitted-item)))
                (insert (concat "@" arg " = " arg))
                (indent-region (line-beginning-position) (line-end-position))
                (newline)
                )))
      )

    (insert "end")
    (indent-region (line-beginning-position) (line-end-position))
    (newline)
    (newline))
  (otavio/delete-current-line)
  (forward-line -1)
  (otavio/delete-current-line)
  (forward-line -2)
  (end-of-line)
  (newline-and-indent))

(after! magit
  ;; Projectile globally with SPC r
  (require 'projectile-rails)
  (map! :leader "r" #'projectile-rails-command-map)

  ;; Fix projectile texts
  (after! which-key
    (push '((nil . "projectile-rails-\\(.+\\)") . (nil . "\\1"))
          which-key-replacement-alist))

  (map! :i :mode ruby-mode-map "<C-M-return>" #'otavio/grb)
  (map! :map ruby-mode-map :localleader "S" 'otavio/split-ruby-giant-string)
  (map! :map ruby-mode-map :localleader "B" 'ruby-toggle-block)
  ;; Better C-j and C-k
  (map! :map ruby-mode-map
        "C-k" #'ruby-beginning-of-block
        "C-j" #'ruby-end-of-block)
  (map! :mode ruby-mode-map :leader "=" #'rubocop-on-current-file)))
