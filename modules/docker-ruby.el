(defvar ruby-docker-compose-command "docker-compose" "Command to use to run docker-compose.")
(defvar ruby-docker-rails-rspec-command "run" "Command to run rspec server with docker.")
(defvar ruby-docker-rails-server-command "up" "Command to start rails server with docker.")
(defvar ruby-docker-rails-console-command "run {{container}} rails console" "Command to start rails console with docker.")

(defvar ruby-docker-rubocop-command "run {{container}} rubocop -a " "Command to run rubocop on current file with docker")
(defvar ruby-docker-compose-cwd "/app/" "CWD of your rails project.")
(defvar ruby-docker-compose-container "web" "Container name of your rails project inside docker-compose.")
(defvar ruby-docker-disable-solargraph t "Disable solargraph when using docker.")

(defvar rubocop-on-current-file-command-on-machine "bundle exec rubocop -a " "Command to revert when disabling ruby-docker-mode")

(defun use-ruby-docker--change-container (full-string)
  (replace-regexp-in-string "{{container}}" ruby-docker-compose-container full-string))

(defun use-ruby-docker--set-rspec ()
  (setq rspec-use-docker-when-possible t)
  (setq rspec-docker-command (concat ruby-docker-compose-command " " ruby-docker-rails-rspec-command))
  (setq rspec-docker-cwd ruby-docker-compose-cwd)
  (setq rspec-docker-container ruby-docker-compose-container)
  (setq minitest-use-docker t)
  (setq minitest-docker-container ruby-docker-compose-container))

(defun use-ruby-docker--set-rails ()
  (setq projectile-rails-custom-console-command (concat
                                                 ruby-docker-compose-command " "
                                                 (use-ruby-docker--change-container ruby-docker-rails-console-command)))
  (setq projectile-rails-custom-server-command (concat
                                                ruby-docker-compose-command " "
                                                (use-ruby-docker--change-container ruby-docker-rails-server-command))))

(defun use-ruby-docker--set-rubocop ()
  (setq rubocop-on-current-file-command (concat ruby-docker-compose-command " " (use-ruby-docker--change-container ruby-docker-rubocop-command)))
  (setq ruby-disabled-checkers '(ruby-reek lsp ruby-rubylint ruby-rubocop)))


(defun disable-ruby-docker--set-rspec ()
  (setq rspec-use-docker-when-possible nil)
  (setq rspec-docker-command nil)
  (setq minitest-use-docker nil))

(defun disable-ruby-docker--set-rails ()
  (setq projectile-rails-custom-console-command nil)
  (setq projectile-rails-custom-server-command nil))

(defun disable-ruby-docker--set-rubocop ()
  (setq rubocop-on-current-file-command rubocop-on-current-file-command-on-machine)
  (setq ruby-disabled-checkers '(ruby-reek lsp ruby-rubylint ruby-rubocop)))

(defun disable-ruby-docker ()
  (interactive)

  (disable-ruby-docker--set-rspec)
  (disable-ruby-docker--set-rubocop)
  (disable-ruby-docker--set-rails)

  (when ruby-docker-disable-solargraph
    (setq lsp-disabled-clients nil))

  (after! flycheck
    (when ruby-docker-disable-solargraph
      (setq lsp-disabled-clients nil)))

  (after! rspec-mode (disable-ruby-docker--set-rspec))
  (after! minitest (disable-ruby-docker--set-rspec))
  (after! projectile-rails (disable-ruby-docker--set-rails))
  (after! flycheck (disable-ruby-docker--set-rubocop))

  (message "Ruby Docker Mode Disabled."))

(defun use-ruby-docker ()
  (interactive)

  (use-ruby-docker--set-rspec)
  (use-ruby-docker--set-rubocop)
  (use-ruby-docker--set-rails)

  (when ruby-docker-disable-solargraph
    (setq lsp-disabled-clients '(solargraph)))

  (after! flycheck
    (when ruby-docker-disable-solargraph
      (setq lsp-disabled-clients '(solargraph))))

  (after! rspec-mode (use-ruby-docker--set-rspec))
  (after! minitest (use-ruby-docker--set-rspec))
  (after! projectile-rails (use-ruby-docker--set-rails))
  (after! flycheck (use-ruby-docker--set-rubocop))

  (message "Ruby Docker Mode Activated."))
