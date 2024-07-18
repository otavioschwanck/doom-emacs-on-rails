;; MOST IMPORTANT CONFIG - Set your project folders
(setq projectile-project-search-path '("~/Projetos"))

(setq user-full-name "Otávio Schwanck dos Santos"
      user-mail-address "otavioschwanck@gmail.com")

;; If you use macos with rbenv on homebrew, add it, uncomment it
;; (setq rbenv-executable "/usr/local/opt/rbenv/bin/rbenv") ;; Rosetta (intel emulation)
;; (setq rbenv-executable "/opt/homebrew/bin/rbenv") ;; Arm (normal brew)

;; Set your theme
(setq doom-theme 'doom-one)

;; My recommendation is JetBrains Mono.  Use M-x reload-user-settings to see the font change.
;; (setq doom-font (font-spec :family "JetBrains Mono" :size 13 :weight 'bold) ;; You can change to regular if you prefer
;;       doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 13))

;; How do you want to display lien numbers?
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; TERMINAL MANAGEMENT STUFF ;;
;; Predefined commands
;; You can switch to any terminal with SPC l
;; You can search the commands with SPC o t
;; You can send any text to any terminal by selecting and pressing SPC l
;; You can quickly execute a define command with SPC j + the keybinding you defined.

(require 'which-key) ;; Needed for which-key to work
(after! which-key
  ;;                         | Name              | command                      | Keybinding |
  (+add-command-to-term-list '("Docker Compose" . "docker-compose up") "u") ;; SPC j u

  ;; Example asking something
  (+add-command-to-term-list '("Add Yarn Package" . (concat "yarn add " (read-string "Package name: "))) "ya") ;; SPC j y a

  ;; Example of dynamic command (using buffer name as example)
  (+add-command-to-term-list '("Rspec on file" . (concat "bundle exec rspec " (buffer-file-name))) "sv") ;; SPC j s v
  (+add-command-to-term-list '("Rspec on line" . (concat "bundle exec rspec " (buffer-file-name) ":" (format "%s" (line-number-at-pos)))) "ss") ;; SPC j s s

  ;; Getting text and executing a command
  (+add-command-to-term-list '("Brownie Test" . (concat "brownie test -k " (save-excursion (search-backward "def test_") (forward-word 2) (thing-at-point 'symbol t)))) "bt") ;; SPC j b t

  ;; Running scripts of a specific folder
  (+add-command-to-term-list '("Brownie Run Script" . (concat "brownie run " (read-file-name "scripts/") " " (read-string "Extra parameters: " nil "commands"))) "br") ;; SPC j b r

  ;; Creating terminal layouts: SPC T
  ;; It will create a new workspace with all terminals listed
  ;;                         | Layout Name    | Commands to execute                |
  (+add-layout-to-term-list '("Rails" . '("rails console" "rails server" nil)))
  (+add-layout-to-term-list '("React" . '("yarn start" nil)))
  (+add-layout-to-term-list '("Next JS" . '("yarn dev" "cowsay 'Have an nice work'" nil)))
  )

;; Harpoon separate by branch? (Harpoon leader key: ,)
(setq harpoon-separate-by-branch nil)

;; Ignoring some folders on search
(after! projectile
  (setq projectile-globally-ignored-directories '("flow-typed" "node_modules" "~/.config/emacs/.local/" ".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".ccls-cache" ".cache" ".clangd")))

;; Add your custom searches (in rails folders)
(after! projectile-rails
  (doom-emacs-on-rails-add-custom-projectile-finder "services" "app/services/"  "\\(.+\\)\\.rb$" "app/services/${filename}.rb" "rt")
  (doom-emacs-on-rails-add-custom-projectile-finder "admin" "app/admin/"  "\\(.+\\)\\.rb$" "app/admin/${filename}.rb" "rt")
  (doom-emacs-on-rails-add-custom-projectile-finder "contracts" "app/contracts/"  "\\(.+\\)\\.rb$" "app/contracts/${filename}.rb" "rq"))

;; fix your identation level for stuff?
(setq js-indent-level 2)
(setq ts-indent-level 2)
(setq typescript-indent-level 2)
(setq ruby-indent-level 2)
(setq standard-indent 2)

;; Google Tradutor, source and target languages
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "pt")


;; Rubocop on current file command
(setq rubocop-on-current-file-command "bundle exec rubocop -A ") ;; SPC =

;; Disable Rubocop or any other lint if you want.  Linter list on: SPC h v flycheck-checkers
;; (add-hook 'ruby-mode-hook
;;  (lambda ()
;;    (setq-local flychech-checker nil)
;;    (setq-local flycheck-disabled-checkers '(ruby-reek lsp ruby-rubylint ruby-rubocop))) 1000)

;; Do you use minitest instead o rspec
;; (use-minitest "_test")

;; Use sorbet instead solargraph?
;; (after! lsp-mode
;;   (setq lsp-disabled-clients '(ruby-ls solargraph))
;;   (setq lsp-sorbet-use-bundler t))

;; Stop some boring warnings
(setq warning-minimum-level :emergency)

;; Start projectile with magit, uncomment below:
;; (after! projectile
;;   (defun open-projectile-with-magit (&optional DIRECTORY CACHE)
;;     (interactive)
;;     (magit-status DIRECTORY)
;;     (if (fboundp 'magit-fetch-from-upstream)
;;         (call-interactively #'magit-fetch-from-upstream)
;;       (call-interactively #'magit-fetch-current)))
;;   (setq +workspaces-switch-project-function #'open-projectile-with-magit))

;; Build your own file switches here
;; (after! projectile-rails
;;   ;; Example: switch from app/contracts/{resource}.rb to app/services/{resource} and vice-versa
;;   (defun projectile-rails-find-contract ()
;;     "Switch from contract to service and vice versa."
;;     (interactive)
;;     (if (string-match-p "app/contracts" (buffer-file-name)) (find-file (replace-regexp-in-string "contract" "service" (replace-regexp-in-string "_contracts" "_services" (buffer-file-name))))
;;       (find-file (replace-regexp-in-string "service" "contract" (replace-regexp-in-string "_services" "_contracts" (buffer-file-name))))))
;;   (map! :leader "rQ" #'projectile-rails-find-contract) ;; Uncomment to bind to SPC r q
;;   )


;; Want to use DOCKER?
;; First, configure you docker variables:

;; (load (expand-file-name "modules/docker.el" doom-private-dir))

;; (setq ruby-docker-compose-command "docker-compose") ;; You docker-compose command (tip: you can use "cd ../; docker-compose")
;; (setq ruby-docker-rails-server-command "up") ;' To start rails server with SPC r R (docker-compose is implicit)
;; (setq ruby-docker-rails-console-command "run {{container}} rails console") ;; to start rails console (docker-compose is implicit)

;; (setq ruby-docker-rubocop-command "run {{container}} rubocop -a ") ;; Command to run rubocop on current file with SPC =
;; (setq ruby-docker-compose-cwd "/app/")
;; (setq ruby-docker-compose-container "web")

;; Tip here:  You can use M-x rbenv-use and select one version that has solargraph.  You can also install with apt or brew.
;; (setq ruby-docker-disable-solargraph nil) ;; If you want to disable solargraph, change to t.  PS:  You can use solargraph by removing .ruby-version of your project and using from rbenv.
;; (use-ruby-docker)
;;
;; End Docker

;; Change Javascript autoformat
(setq-hook! 'rjsx-mode-hook +format-with 'prettier)
(setq-hook! 'typescript-tsx-mode-hook +format-with 'prettier)
(setq-hook! 'typescript-mode-hook +format-with 'prettier)
