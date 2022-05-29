(defun load-libraries ()
  (load (expand-file-name "modules/libraries/rubocop.el" doom-private-dir))
  (load (expand-file-name "modules/libraries/http.el" doom-private-dir))
  (map! :leader :desc "Rails" "r" #'projectile-rails-command-map)
  (require 'projectile-rails))

(add-hook! 'doom-first-buffer-hook 'load-libraries)

(after! evil
  (defadvice
      evil-ex-search
      (after evil-search-forward-recenter activate)
    (recenter))
  (ad-activate 'evil-ex-search))

(after! evil-mc
  (global-evil-mc-mode -1))

(after! rotate-text
  (add-to-list 'rotate-text-words '("valid" "invalid"))
  (add-to-list 'rotate-text-words '("context" "describe"))
  (add-to-list 'rotate-text-symbols '("be_valid" "be_invalid"))
  (add-to-list 'rotate-text-symbols '("valid?" "invalid?"))
  (add-to-list 'rotate-text-symbols '("present?" "blank?" "nil?"))
  (add-to-list 'rotate-text-symbols '("belongs_to" "has_many" "has_one"))
  (add-to-list 'rotate-text-symbols '("if" "unless"))
  (add-to-list 'rotate-text-symbols '("greater_than" "greater_than_or_equal_to" "equal_to" "less_than" "less_than_or_equal_to" "other_than" "odd" "even"))
  (add-to-list 'rotate-text-symbols '("to" "not_to")))

(after! ruby-mode
  (set-lookup-handlers! 'ruby-mode
    :definition '(projectile-rails-goto-file-at-point robe-jump)
    :documentation #'robe-doc)

  (defvar rails-reset-command "rails db:environment:set RAILS_ENV=development; rails db:drop db:create db:migrate;rails db:seed"
    "Command to reset rails")

  (defun otavio/kill-ruby-instances ()
    (interactive)
    (async-shell-command "killall -9 rails ruby spring bundle; echo 'Ruby Instances Killed!'" "*Ruby Kill Output*"))

  (defun otavio/reset-rails-database ()
    (interactive)
    (message "Rails database is being reseted!")
    (async-shell-command (concat rails-reset-command "; echo 'Rails database reseted, please close this popup'" )"*Ruby Reset Output*")
    (+popup/raise "*Ruby Reset Output*"))

  (set-popup-rule! "^\\*\\(Ruby Kill Output\\)?" :ttl nil)
  (set-popup-rule! "^\\*\\(Ruby Reset Output\\)?" :ttl nil)

  (defun otavio/rails-reset-all ()
    (interactive)
    (otavio/kill-ruby-instances)
    (otavio/reset-rails-database))

  (map! :after ruby-mode :mode ruby-mode :localleader :desc "Ruby Reset" "w")
  (map! :after ruby-mode :mode ruby-mode :localleader :desc "Reset Database" "ww" #'otavio/rails-reset-all)
  (map! :after ruby-mode :mode ruby-mode :localleader :desc "Kill All Ruby Instances" "wk" #'otavio/kill-ruby-instances))

(after! web-mode
  (set-lookup-handlers! 'web-mode
    :definition '(projectile-rails-goto-file-at-point rails-routes-jump)))

(after! projectile-rails
  (defun doom-emacs-on-rails-add-custom-projectile-finder (name folder file-pattern pattern keybinding)
    (fset (intern (concat "projectile-rails-custom-find-" name))
          (eval `
           (lambda ()
             (interactive)
             (projectile-rails-find-resource
              (concat ,name ": ")
              '((,folder ,file-pattern))
              ,pattern))))
    (map! :leader :desc (concat "Find " name) keybinding (intern (concat "projectile-rails-custom-find-" name)))))

;; Rspec Stuff
(after! rspec-mode
  (set-popup-rule! "^\\*\\(rspec-\\)?compilation" :size 0.5 :ttl nil :select t)
  (map! :leader :desc "Rspec" "t" #'rspec-mode-keymap)
  (map! :leader :desc "Run Last Failed" "tl" #'rspec-run-last-failed))

(after! ruby-mode
  (map! :mode ruby-mode :leader :desc "Go to Test" "a" 'goto-test)
  (map! :mode ruby-mode :leader :desc "Go to Test and split" "A" 'goto-test-and-vsplit)

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
    (if (string-match-p "/app/" buffer-file-name) (find-file (file-path-to-test buffer-file-name))))

  (defun goto-test ()
    (interactive)
    (find-file (file-path-to-test buffer-file-name))))

;; Rubocop
(defun project-has-rubocop ()
  (let ((found nil))
    (cl-block find-rubocop
      (mapc (lambda (line) (when (string-match "rubocop" line) (setq found t) (cl-return-from find-rubocop)))
            (with-temp-buffer
              (insert-file-contents (concat (projectile-project-root) "Gemfile.lock"))
              (split-string (buffer-string) "\n" t))))
    found))

(defvar rubocop-append-command '("bundle" "exec")
  "Commands to run before rubocop")

(defvar disabled-checkers '("bundle" "exec")
  "Commands to run before rubocop")

;; Disable for ruby
(setq-hook! 'ruby-mode-hook +format-with-lsp nil)

(add-hook 'ruby-mode-hook
          (lambda ()
            (if (and (not (eq (projectile-project-root) nil)) (file-exists-p (concat (projectile-project-root) "Gemfile.lock")) (project-has-rubocop))
                (progn
                  (setq-local flycheck-checker 'ruby-rubocop)
                  (setq-local flycheck-command-wrapper-function
                              (lambda (command) (append rubocop-append-command command))))

              (setq-local flycheck-disabled-checkers '(ruby-reek ruby-rubylint ruby-rubocop)))))

(defvar ruby-disabled-checkers '(ruby-reek lsp ruby-rubylint) "Checkers to automatically disable on ruby files.")

(add-hook! 'ruby-mode-hook (setq-local flycheck-disabled-checkers ruby-disabled-checkers))

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

  ;; binding it to SPC m c
  (map! :map ruby-mode-map :desc "Copy Class Name" :localleader "c" #'endless/ruby-copy-class-name))

;; Rails Routes Plugin
(after! web-mode
  (define-key web-mode-map (kbd "C-c o") #'rails-routes-insert)
  (define-key web-mode-map (kbd "C-c C-o") #'rails-routes-insert-no-cache))

(after! ruby-mode
  (map! :mode ruby-mode "C-c o" #'rails-routes-insert)
  (map! :mode ruby-mode "C-c C-o" #'rails-routes-insert-no-cache))

(after! evil
  (define-key evil-normal-state-map (kbd "g a") #'rails-routes-jump)
  (define-key evil-visual-state-map (kbd "g a") #'rails-routes-jump))

;; Ruby Json to hash
(after! ruby-mode
  (map! :mode ruby-mode :localleader "J" 'ruby-json-to-hash-parse-json) ;; Parse the json, SPC m J
  (map! :mode ruby-mode :localleader "j" 'ruby-json-to-hash-toggle-let)) ;; Create a let or send the let back to parent. SPC m j

;; Ruby Insert I18n
(after! ruby-mode
  (map! :map ruby-mode-map "C-c i" 'rails-i18n-insert-with-cache) ;; Search with cache on ruby mode
  (map! :map ruby-mode-map "C-c C-i" 'rails-i18n-insert-no-cache) ;; Search refresh cache on ruby modee
  (map! :map web-mode-map "C-c i" 'rails-i18n-insert-with-cache) ;; Search with cache on web-mode
  (map! :map web-mode-map "C-c C-i" 'rails-i18n-insert-no-cache)) ;; Search refresh cache web-mode

;; HTTP Plugin
(after! ruby-mode
  (define-key ruby-mode-map (kbd "C-c s") #'rails-http-statuses-insert-symbol)
  (define-key ruby-mode-map (kbd "C-c S") #'rails-http-statuses-insert-code))

;; Improve Inf Ruby
(after! inf-ruby
  (defun inf-ruby-goto-insert ()
    (interactive)
    (goto-char (point-max))
    (when (featurep 'evil)
      (evil-insert 1)))

  (defun inf-ruby-type (text)
    (interactive)
    (inf-ruby-goto-insert)
    (goto-char (point-at-bol))
    (when (word-at-point t) (kill-line t))
    (insert text)
    (comint-send-input))

  (defvar inf-ruby-command-to-continue "continue" "Command used to exit inf ruby")

  (defun inf-ruby-exit ()
    (interactive)
    (inf-ruby-type (if (cl-search "*rails" (buffer-name)) "exit" inf-ruby-command-to-continue)))

  (defun inf-ruby-reload ()
    (interactive)
    (inf-ruby-type "reload!"))

  (defun inf-ruby-step ()
    (interactive)
    (inf-ruby-type "step"))

  (defun inf-ruby-next ()
    (interactive)
    (inf-ruby-type "next"))

  (defun inf-ruby-disable-logger ()
    (interactive)
    (if logger-disabled
        (progn
          (inf-ruby-type "ActiveRecord::Base.logger = old_logger")
          (setq-local logger-disabled nil)
          (message "Logger is back!")
          )
      (progn
        (setq-local logger-disabled t)
        (inf-ruby-type "old_logger = ActiveRecord::Base.logger")
        (inf-ruby-type "ActiveRecord::Base.logger = nil")
        (message "Logger disabled!"))))

  (defun inf-ruby-add-keybindings ()
    (if (cl-search "*rails" (buffer-name))
        (progn
          (message "Ruby Console Tips: Press C-l to send exit, C-M-l to reload, press A to move from normal to insert mode at end, Press C-M-o to disable SQL log."))
      (progn
        (message "Debugging Tips: Press C-l to send continue, press A to move from normal to insert mode at end, C-f to next and C-M-f to step.")))

    (evil-local-set-key 'normal (kbd "A") #'inf-ruby-goto-insert)

    (setq-local logger-disabled nil)

    (evil-local-set-key 'normal (kbd "C-d") #'inf-ruby-exit)
    (define-key evil-insert-state-local-map (kbd "C-d") #'inf-ruby-exit)

    (evil-local-set-key 'normal  (kbd "C-M-l") #'inf-ruby-reload)
    (define-key evil-insert-state-local-map (kbd "C-M-l") #'inf-ruby-reload)

    (evil-local-set-key 'normal  (kbd "C-M-f") #'inf-ruby-step)
    (define-key evil-insert-state-local-map (kbd "C-M-f") #'inf-ruby-step)

    (evil-local-set-key 'normal (kbd "C-f") #'inf-ruby-next)
    (define-key evil-insert-state-local-map (kbd "C-f") #'inf-ruby-next)

    (evil-local-set-key 'normal  (kbd "C-M-o") #'inf-ruby-disable-logger)
    (define-key evil-insert-state-local-map (kbd "C-M-o") #'inf-ruby-disable-logger))

  (add-hook! 'inf-ruby-mode-hook 'inf-ruby-add-keybindings))

(defun popserver-when-on-byebug (_SYMBOL NEWVAL _OPERATION _WHERE)
  (when (and (eq NEWVAL 0) (cl-search "projectile-rails" (buffer-name)))
    (progn (switch-to-buffer-other-window (buffer-name))
           (goto-char (point-max))
           (when (featurep 'evil)
             (evil-insert-state)))))

(add-variable-watcher 'inf-ruby-at-top-level-prompt-p 'popserver-when-on-byebug)

;; Method Generator
(after! ruby-mode
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

  (map! :map ruby-mode-map :i "M-e" #'otavio/grb))

(after! ruby-mode
  (defun otavio/-current-line-empty-p ()
    (save-excursion
      (beginning-of-line)
      (looking-at-p "[[:space:]]*$")))

  (defun otavio/-swap-search-forward-swap-to-singleline (SEARCH)
    (if (search-backward SEARCH (line-beginning-position) t)
        (progn
          (kill-visual-line)
          (forward-line 1)
          (end-of-line)
          (insert " ")
          (yank)
          (indent-according-to-mode)
          (forward-line 1)
          (kill-line)
          (kill-line)
          (forward-line -2)
          (kill-line)
          (forward-to-indentation 0)
          t)))

  (defun otavio/-swap-search-forward-swap-to-multiline (SEARCH)
    (if (search-forward SEARCH (line-end-position) t)
        (progn
          (backward-word)
          (backward-char)
          (kill-visual-line)
          (forward-line -1)
          (if (not (otavio/-current-line-empty-p))
              (progn
                (end-of-line)))
          (newline)
          (yank)
          (indent-according-to-mode)
          (forward-line 1)
          (indent-according-to-mode)
          (end-of-line)
          (newline)
          (insert "end")
          (indent-according-to-mode)
          t)))

  (defun otavio/swap-if-unless-ruby ()
    (interactive)
    (beginning-of-line)
    (forward-word)
    (let ((changed nil))
      (if (not changed)
          (setq changed (otavio/-swap-search-forward-swap-to-multiline " if ")))
      (if (not changed)
          (setq changed (otavio/-swap-search-forward-swap-to-multiline " unless ")))
      (if (not changed)
          (setq changed (otavio/-swap-search-forward-swap-to-singleline "if")))
      (if (not changed)
          (setq changed (otavio/-swap-search-forward-swap-to-singleline "unless")))
      (if (not changed)
          (progn
            (forward-line -1)
            (beginning-of-line)
            (forward-word)))
      (if (not changed)
          (setq changed (otavio/-swap-search-forward-swap-to-singleline "if")))
      (if (not changed)
          (setq changed (otavio/-swap-search-forward-swap-to-singleline "unless")))
      (if (not changed)
          (progn
            (forward-line -1)
            (beginning-of-line)
            (forward-word)))
      (if (not changed)
          (setq changed (otavio/-swap-search-forward-swap-to-singleline "if")))
      (if (not changed)
          (setq changed (otavio/-swap-search-forward-swap-to-singleline "unless")))))

  (map! :map ruby-mode-map :desc "split or join if/unless" :localleader "i" #'otavio/swap-if-unless-ruby))


;; Split Giant String
(defvar split-ruby-giant-string-default 125)

(after! ruby-mode
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
              (newline)
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

  (map! :map ruby-mode-map :localleader :desc "Split giant string" "S" #'otavio/split-ruby-giant-string))


;; Method Refactor
(defvar ruby-rspec-describe-class "call")

(defun use-minitest (test-indicator)
  "Use Minitest instead of rails"
  (interactive)
  (after! ruby-mode ;; Beggining of minitest-code
    (defun goto-test ()
      (interactive)
      (find-file (file-path-to-test buffer-file-name)))
    (defun goto-test-and-vsplit ()
      (interactive)
      (if (string-match-p "/test/" buffer-file-name) (find-file (file-path-to-test buffer-file-name)))
      (delete-other-windows)
      (evil-window-vsplit)
      (if (string-match-p "/app/" buffer-file-name) (find-file (file-path-to-test buffer-file-name))))

    (defun file-path-to-test (filename)
      (if (string-match-p "/test/" filename)
          (if (string-match-p "/admin/" filename)
              (concat
               (replace-regexp-in-string "/test/controllers/" "/app/" (file-name-directory filename))
               (singularize-string (replace-regexp-in-string "_controller_test" "" (file-name-base filename)))
               "."
               (file-name-extension filename))
            (concat
             (replace-regexp-in-string "/test/" "/app/" (file-name-directory filename))
             (replace-regexp-in-string test-indicator "" (file-name-base filename))
             "."
             (file-name-extension filename)))
        (if (string-match-p "/admin/" filename)
            (concat
             (replace-regexp-in-string "/app/" "/test/controllers/" (file-name-directory filename))
             (pluralize-string (file-name-base filename))
             "_controller_test."
             (file-name-extension filename))
          (concat
           (replace-regexp-in-string "/app/" "/test/" (file-name-directory filename))
           (file-name-base filename)
           "_test."
           (file-name-extension filename)))))

    (after! rspec-mode
      (map! :mode ruby-mode :leader "tv" #'minitest-verify)
      (map! :mode ruby-mode :leader "ts" #'minitest-verify-single)
      (map! :mode ruby-mode :leader "tr" #'minitest-rerun)
      (map! :mode ruby-mode :leader "ta" #'minitest-verify-all))))

(after! ruby-mode
  (defun msc/revert-buffer-noconfirm ()
    "Call `revert-buffer' with the NOCONFIRM argument set."
    (interactive)
    (revert-buffer nil t))

  (defvar rubocop-on-current-file-command "bundle exec rubocop -a "
    "Command to execute to fix current file with rubocop")

  (defun rubocop-on-current-file ()
    "RUBOCOP ON CURRENT_FILE."
    (interactive)
    (save-buffer)
    (message "%s" (shell-command-to-string
                   (concat rubocop-on-current-file-command
                           (shell-quote-argument (buffer-file-name)))))
    (msc/revert-buffer-noconfirm))

  (map! :map ruby-mode-map :desc "Add rubocop:disable at point" :localleader "d" 'rubocop-toggle-at-point)
  (map! :mode ruby-mode :desc "Run Rubocop at current file" :leader "=" #'rubocop-on-current-file))

;; Method Refactor
(after! ruby-mode
  (defun ruby-extract-function ()
    (interactive)
    (let* ((function-name (read-string "Method name? "))
           (has-private (ruby-new-method-from-symbol-at-point-verify-private))
           (args (read-string "Arguments without paranthesis (leave blank for no parameters): ")))

      (when (not (string= function-name ""))
        (call-interactively 'evil-change)
        (call-interactively 'evil-normal-state)
        (ruby-extract-function--create-function function-name args has-private)
        (ruby-extract-function--insert-function function-name args))))

  (defun ruby-extract-function--insert-function (function-name args)
    (when (not (eq (point) (point-at-eol)))
      (evil-forward-char))
    (insert function-name)
    (when (not (string= args ""))
      (insert "(" args ")"))
    (evil-indent (point-at-bol) (point-at-eol)))

  (defun ruby-extract-function--create-function (function-name args has-private)
    (save-excursion
      (if (and has-private (yes-or-no-p "private found, create method after private?"))
          (progn
            (search-forward "private\n" (point-max) t)
            (+evil/insert-newline-below 1)
            (forward-line 1))
        (progn
          (+evil/next-end-of-method)
          (when (not (string= (string (following-char)) "\n"))
            (+evil/insert-newline-above 1))
          (+evil/insert-newline-below 1)
          (forward-line 1)))
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

  (map! :mode ruby-mode :localleader :desc "Extract Function" "m" #'ruby-extract-function))

;; Create method at point
(after! ruby-mode
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
           (has-private (ruby-new-method-from-symbol-at-point-verify-private))
           (arguments (ruby-new-method-from-symbol-at-point--get-arguments has-arguments variable-end-point)))
      (ruby-new-method-from-symbol-at-point--create-method variable-name (string-join (remove nil arguments) ", ") has-private)))

  (defun ruby-new-method-from-symbol-at-point-verify-private ()
    (save-excursion
      (search-forward "private\n" (point-max) t)))

  (defun ruby-new-method-from-symbol-at-point--create-method (function-name args has-private)
    (if (and has-private (yes-or-no-p "private found, create method after private?"))
        (progn
          (goto-char (point-min))
          (search-forward "private\n" (point-max))
          (+evil/insert-newline-below 1)
          (forward-line 1))
      (progn
        (+evil/next-end-of-method)
        (when (not (string= (string (following-char)) "\n"))
          (+evil/insert-newline-above 1))
        (+evil/insert-newline-below 1)
        (forward-line 1)))
    (insert "def " function-name)
    (when (not (string= args ""))
      (insert "(" args ")"))
    (evil-indent (point-at-bol) (point-at-eol)) (+evil/insert-newline-below 1) (forward-line 1)
    (insert "end") (evil-indent (point-at-bol) (point-at-eol))
    (+evil/insert-newline-below 1)
    (forward-line -1) (goto-char (point-at-eol)) (newline-and-indent)
    (when (featurep 'evil)
      (evil-insert 1))
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
             (ruby-new-method-from-symbol-at-point--verify-exist argument))
           ) (mapcar 'string-trim (split-string (replace-regexp-in-string "(.*)" "(...)" args-raw) ","))))))

  (defun ruby-new-method-from-symbol-at-point--verify-exist (argument)
    (save-excursion
      (if (or (search-backward-regexp (concat "def " argument "\\(\(\\|$\\)") (point-min) t)
              (search-forward-regexp (concat "def " argument "\\(\(\\|$\\)") (point-max) t))
          nil
        (if (eq 0 (length (let ((case-fold-search nil))
                            (remove "" (split-string argument "[a-z]+\\(_[a-z]+\\)*")))))
            (if (or (string= argument "false")
                    (string= argument "true"))
                (read-string (concat "name for " argument " boolean:  ")) argument)
          (read-string (concat "name for " argument " expression:  "))))))

  (map! :mode ruby-mode :localleader :desc "New method from text at point" "n" #'ruby-new-method-from-symbol-at-point))
