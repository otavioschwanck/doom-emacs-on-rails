;;; ~/.doom.d/no_evil.el -*- lexical-binding: t; -*-

;; Custom Window Keybindings
(global-set-key (kbd "M-o") 'ace-window)

(defun current-mode-company-mode ()
  (interactive)
  (if (eq major-mode 'ruby-mode) (progn (robe-start) (call-interactively 'company-robe))) (when-let (backend (nth 1 company-backends))
    (company-begin-backend (nth 1 company-backends))))

;; Custom file keybindings
(map! "<C-tab>" #'+ivy/switch-workspace-buffer)
(map! "C-S-g" #'magit-status)
(map! "C-j" #'+ivy/projectile-find-file)

;; Custom Editing Keybindings
(global-set-key (kbd "C-o") (kbd "C-e C-m"))
(global-set-key (kbd "C-;") (kbd "C-/"))
(global-set-key (kbd "M-;") #'undo-fu-only-redo)
(global-set-key (kbd "C-S-o") (kbd "C-p C-e C-m"))
(global-set-key (kbd "C-S-k") 'sp-kill-hybrid-sexp)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "C-ç") (kbd "C-1 C-x s"))

(map! "C-c SPC" #'counsel-mark-ring)
(map! "M-2" #'er/expand-region)
(map! "C-c s c" #'avy-goto-char-2)
(map! "<C-return>" #'dabbrev-expand)
(map! "C-S-j" #'current-mode-company-mode)
(map! "C-q" #'yas-expand)

(after! robe
  (map! :map ruby-mode-map "C-." #'otavio/better-ruby-goto-definition))

(map! "C-x k" #'kill-this-buffer)
(map! "C-c e" #'+treemacs/toggle)
(map! "C-c E" #'treemacs-find-file)
(map! "<C-S-return>" #'current-mode-company-mode)

;; Drag stuff rules
(map! "M-p" #'drag-stuff-up)
(map! "M-n" #'drag-stuff-down)
(map! "M-a" #'avy-goto-char-2)

;; (setq avy-all-windows t)
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(map! "M-N" #'duplicate-current-line-or-region)

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
      "RET" #'newline-and-indent
      "<return>" #'newline-and-indent
      "<C-S-return>" #'current-mode-company-mode
      "<tab>" #'company-complete-selection
      "<C-return>" #'dabbrev-expand
      "<C-S-return>" #'company-dabbrev
      "C-q" #'yas-expand)

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
      "C-S-q" 'yas-prev-field
      "C-d" 'yas-skip-and-clear-field
      "C-q" 'yas-next-and-close-company)

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

;; When popping the mark, continue popping until the cursor
;; actually moves
(defadvice pop-to-mark-command
    (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        ad-do-it))))

(setq set-mark-command-repeat-pop t)

(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))
(advice-add 'pop-to-mark-command :around
            #'modi/multi-pop-to-mark)

(after! robe
  (set-company-backend! 'ruby-mode '(company-dabbrev-code :separate company-yasnippet) 'company-robe 'company-yasnippet))

(after! inf-ruby
  (set-company-backend! 'inf-ruby-mode 'company-dabbrev-code 'company-dabbrev 'company-yasnippet))
