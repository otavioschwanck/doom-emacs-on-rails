;;; ~/.doom.d/no_evil.el -*- lexical-binding: t; -*-

;; Custom Window Keybindings
(global-set-key (kbd "M-o") 'ace-window)

(defun current-mode-company-mode ()
  (interactive)
  (if (eq major-mode 'ruby-mode) (progn (robe-start) (call-interactively 'company-robe))) (when-let (backend (nth 1 company-backends))
    (company-begin-backend (nth 1 company-backends))))

;; Custom file keybindings
(map! "<C-tab>" #'+vertico/switch-workspace-buffer)
(map! "C-S-g" #'magit-status)
(map! "C-j" #'projectile-find-file)

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

(after! robe
  (map! :map ruby-mode-map "C-." #'+lookup/definition))

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

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun expand-snippet-or-next ()
  (interactive)
  (if (or (not yas/minor-mode)
      (null (do-yas-expand))
      (company-abort))
      (do-yas-expand)))

(map! :after company
      :map company-active-map
      "<tab>" #'expand-snippet-or-next
      "<C-S-return>" #'current-mode-company-mode
      "<C-return>" #'dabbrev-expand
      "C-q" #'company-complete)

(setq mark-ring-max 10)
(setq global-mark-ring-max 10)
(setq set-mark-command-repeat-pop t)

(after! company
  (defadvice! +company--abort-previous-a (&rest _)
    :before #'company-begin-backend
    (company-abort)))

(defun yas-next-and-close-company ()
  (interactive)
  (if (company--active-p)
      (company-complete-selection))
  (yas-next-field))

(map! :after yasnippet
      :map yas-keymap
      "C-d" 'yas-skip-and-clear-field
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
  (set-company-backend! 'inf-ruby-mode 'company-dabbrev-code 'company-capf 'company-yasnippet))

(setq emmet-expand-jsx-className? nil)

(defun update-yas-indentation ()
  (setq-local yas-indent-line 'fixed))

(defun set-emmet-class-name ()
  (setq-local emmet-expand-jsx-htmlFor? t)
  (setq-local emmet-expand-jsx-className? t))

(setq lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))

(defun set-js-company ()
  (setq lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
  (setq-local +lsp-company-backends '(:separate company-capf company-dabbrev-code company-yasnippet)))

(add-hook! 'rjsx-mode-hook 'set-emmet-class-name)
(add-hook! 'yaml-mode-hook 'update-yas-indentation)
(add-hook! 'rjsx-mode-hook 'set-js-company)

(after! lsp-mode
  (setq lsp-auto-guess-root t)
  (setq lsp-solargraph-formatting nil)
  (setq lsp-solargraph-symbols nil)
  (setq lsp-solargraph-folding nil))

(after! robe
  (set-lookup-handlers! 'ruby-mode
    :definition '(projectile-rails-goto-file-at-point robe-jump)
    :documentation #'robe-doc))

(after! ruby-mode
  (set-lookup-handlers! 'ruby-mode
    :definition '(projectile-rails-goto-file-at-point robe-jump)
    :documentation #'robe-doc))

(after! web-mode
  (set-lookup-handlers! 'web-mode
    :definition '(projectile-rails-goto-file-at-point rails-routes-jump)))

(defun ruby-extract-function ()
  (interactive)
  (let* ((function-name (read-string "Method name? "))
         (args (read-string "Arguments without paranthesis (leave blank for no parameters): ")))

    (when (not (string= function-name ""))
      (call-interactively 'evil-change)
      (call-interactively 'evil-normal-state)
      (ruby-extract-function--create-function function-name args)
      (ruby-extract-function--insert-function function-name args)
      )))

(defun ruby-extract-function--insert-function (function-name args)
  (when (not (eq (point) (point-at-eol)))
    (evil-forward-char))
  (insert function-name)
  (when (not (string= args ""))
    (insert "(" args ")"))
  (evil-indent (point-at-bol) (point-at-eol)))

(defun ruby-extract-function--create-function (function-name args)
  (save-excursion
    (+evil/next-end-of-method)
    (when (not (string= (string (following-char)) "\n"))
      (+evil/insert-newline-above 1))
    (+evil/insert-newline-below 1)
    (forward-line 1)
    (insert "def " function-name)
    (when (not (string= args ""))
      (insert "(" args ")"))
    (evil-indent (point-at-bol) (point-at-eol)) (+evil/insert-newline-below 1) (forward-line 1)
    (insert "end") (evil-indent (point-at-bol) (point-at-eol))
    (+evil/insert-newline-above 1) (+evil/insert-newline-below 1)
    (forward-line -1)
    (evil-paste-after 1)
    (forward-line -1)
    (when (string= (string (following-char)) "\n") (delete-char 1))
    (+evil/reselect-paste)
    (call-interactively 'evil-indent)))

(map! :after ruby-mode :mode ruby-mode :localleader "m" #'ruby-extract-function)

(defun ruby-new-method-from-symbol-at-point ()
  (interactive)
  (better-jumper-set-jump)
  (when (looking-at-p "\\sw\\|\\s_")
    (forward-sexp 1))
  (forward-sexp -1)
  (let* ((variable-start-point (point))
         (variable-end-point nil)
         (variable-name (save-excursion (forward-sexp 1) (setq variable-end-point (point)) (buffer-substring-no-properties variable-start-point (point))))
         (has-arguments (save-excursion (goto-char variable-end-point) (looking-at-p "(")))
         (arguments (ruby-new-method-from-symbol-at-point--get-arguments has-arguments variable-end-point))
         )
    (ruby-new-method-from-symbol-at-point--create-method variable-name (string-join (remove nil arguments) ", "))
    ))

(defun ruby-new-method-from-symbol-at-point--create-method (function-name args)
  (+evil/next-end-of-method)
  (when (not (string= (string (following-char)) "\n"))
    (+evil/insert-newline-above 1))
  (+evil/insert-newline-below 1)
  (forward-line 1)
  (insert "def " function-name)
  (when (not (string= args ""))
    (insert "(" args ")"))
  (evil-indent (point-at-bol) (point-at-eol)) (+evil/insert-newline-below 1) (forward-line 1)
  (insert "end") (evil-indent (point-at-bol) (point-at-eol))
  (+evil/insert-newline-above 1) (+evil/insert-newline-below 1)
  (forward-line -1)
  (when (featurep 'evil)
    (evil-change (point) (point))) (indent-for-tab-command)
  (message "Method created!  Pro Tip:  Use C-o (normal mode) to jump back to the method usage."))

(defun ruby-new-method-from-symbol-at-point--get-arguments (has-arguments variable-end-point)
  (when has-arguments
    (let* ((start-args-point nil)
           (end-args-point nil)
           (args-raw nil)
           )
      (save-excursion (goto-char variable-end-point) (evil-forward-word-begin) (setq start-args-point (point)) (evil-backward-word-end)
                      (evil-jump-item)
                      (setq end-args-point (point)))
      (setq args-raw (buffer-substring-no-properties start-args-point end-args-point))
      (mapcar
       (lambda (argument)
         (if (string-match-p "(...)" argument)
             (read-string (concat "name for " argument " argument:  "))
           (if (string= (substring argument 0 1) "@") nil (ruby-new-method-from-symbol-at-point--verify-exist argument)))

         ) (mapcar 'string-trim (split-string (replace-regexp-in-string "(.*)" "(...)" args-raw) ","))))))

(defun ruby-new-method-from-symbol-at-point--verify-exist (argument)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp (concat "def " argument "\\(\(\\|$\\)") (point-max) t)
        nil
      (if (eq 0 (length (let ((case-fold-search nil))
                          (remove "" (split-string argument "[a-z]+\\(_[a-z]+\\)*"))))) argument
        (read-string (concat "name for " argument " expression:  "))))))

(map! :after ruby-mode :mode ruby-mode :localleader "n" #'ruby-new-method-from-symbol-at-point)

(defun j-company-remove-dabbrev-dups-keep-order (candidates)
  "Loop over CANDIDATES and remove duplicate candidates if they belong to
  `company-dabbrev' or `company-dabbrev-code'."
  (let ((hash (make-hash-table :test 'equal :size (length candidates)))
        (new-list nil))
    (dolist (candidate candidates)
      (let ((stripped-candidate (substring-no-properties candidate))
            (candidate-backend (get-text-property 0 'company-backend candidate)))
        (cond
         ;; Candidate is `company-yasnippet', always push this.
         ((eq (get-text-property 0 'company-backend candidate)
              'company-yasnippet)
          (push candidate new-list))
         ;; Candidate has not been seen.
         ((not (gethash stripped-candidate hash))
          (puthash stripped-candidate candidate hash)
          (push candidate new-list))
         ;; Candidate has been seen.
         ;; `company-dabbrev' or `company-dabbrev-code' is the candidate.
         ((or candidate-backend
              (eq candidate-backend 'company-dabbrev-code)
              (eq candidate-backend 'company-dabbrev))
          t)
         ;; Candidate has been seen but is not `company-dabbrev'
         ;; or `company-dabbrev-code'.
         (:seen-but-candidate-not-dabbrev
          ;; If the candidate in the hash table is dabbrev, replace it.
          ;; Otherwise, we are fine with duplicates as long as the backends
          ;; are meaningful.
          (let* ((hash-candidate (gethash stripped-candidate hash))
                 (hash-backend (get-text-property
                                0 'company-backend hash-candidate)))
            (if (or hash-backend
                    (eq hash-backend 'company-dabbrev)
                    (eq hash-backend 'company-dabbrev-code))
                (progn
                  (setcar
                   (nthcdr
                    (cl-position hash-candidate new-list :test 'eq)
                    new-list)
                   candidate)
                  (puthash stripped-candidate candidate hash)
                  t)
              ;; Only need one candidate in the hash table.
              (push candidate new-list)))))))
    (reverse new-list)))

(after! company
  (set-company-backend! 'inf-ruby-mode 'company-dabbrev-code)
  (setq +lsp-company-backends '(:separate company-capf company-dabbrev-code company-yasnippet))

  (add-to-list 'company-transformers
               #'j-company-remove-dabbrev-dups-keep-order))
