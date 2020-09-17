;;; ~/.doom.d/no_evil.el -*- lexical-binding: t; -*-

;; Custom Window Keybindings
(global-set-key (kbd "M-o") 'ace-window)

;; Custom file keybindings
(map! "<C-tab>" #'+ivy/switch-workspace-buffer)
(map! "C-S-g" #'magit-status)
(map! "C-q" #'+ivy/projectile-find-file)

;; Custom Editing Keybindings
(global-set-key (kbd "C-o") (kbd "C-e C-m"))
(global-set-key (kbd "C-S-o") (kbd "C-p C-e C-m"))
(global-set-key (kbd "C-S-k") 'sp-kill-hybrid-sexp)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "C-ç") (kbd "C-1 C-x s"))

(map! "M-2" #'er/expand-region)
(map! "C-u" #'undo)
(map! "C-c s c" #'avy-goto-char-2)
(map! "<C-return>" #'dabbrev-expand)
(map! "<C-S-return>" #'+company/dabbrev)
(map! "<C-M-return>" #'company-yasnippet)
(map! "C-." #'+lookup/definition)
(map! "C-x k" #'kill-this-buffer)
(map! "C-M-;" #'+neotree/open)

;; Drag stuff rules
(map! "M-p" #'drag-stuff-up)
(map! "M-n" #'drag-stuff-down)
(map! "M-a" #'avy-goto-char-2)

(setq avy-all-windows t)

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
    With argument N, make N copies.
    With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-char pos)))))
(forward-line 1)

(map! "M-N" #'duplicate-line-or-region)

;; (define-key org-mode-map (kbd "C-c l <return>") #'+org/dwim-at-point)

(defun save-all ()
  "Save all files"
  (interactive)
  (save-all-buffers)
  (delete-trailing-whitespace))

;; Custom Projectile Keybindings
(map! "C-c p w" #'projectile-run-shell)
(global-set-key (kbd "C-ç") #'save-all)

(use-package! multiple-cursors
  :init
  (global-set-key (kbd "C-c f c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(after! ace-window
  (setq aw-keys '(?o ?p ?k ?i ?j ?h ?u ?y ?h ?g)))

(map! "C-+" #'indent-whole-buffer)

(after! magit
  (with-eval-after-load 'magit-mode
    (define-key magit-mode-map (kbd "<C-tab>") nil))
  )

(map! :mode magit-status-mode-map "<C-tab>" #'+ivy/switch-workspace-buffer)

(map! :after company
      :map company-active-map
      "<tab>" #'yas-expand)

(setq mark-ring-max 10)
(setq global-mark-ring-max 10)
(setq set-mark-command-repeat-pop t)

(after! company
  (defadvice! +company--abort-previous-a (&rest _)
    :before #'company-begin-backend
    (company-abort)))

(defun yas-next-and-close-company ()
  (interactive)
  (company-abort)
  (yas-next-field))

(map! :after yasnippet
      :map yas-keymap
      "<tab>" 'yas-next-and-close-company)

(after! ruby-mode
  (defun msc/revert-buffer-noconfirm ()
    "Call `revert-buffer' with the NOCONFIRM argument set."
    (interactive)
    (revert-buffer nil t))

  (defun rubocop-on-current-file ()
    "RUBOCOP ON CURRENT_FILE."
    (interactive)
    (save-all-buffers)
    (message "%s" (shell-command-to-string
                   (concat "bundle exec rubocop -a "
                           (shell-quote-argument (buffer-file-name)))))
    (msc/revert-buffer-noconfirm))
  (define-key ruby-mode-map (kbd "C-)") #'rubocop-on-current-file))
