;; Load only after ruby mode
(load (expand-file-name "plugins/miyagi.el" doom-private-dir))

(after! ruby-mode
  (load (expand-file-name "plugins/rubocop.el" doom-private-dir))
  (load (expand-file-name "plugins/rails-http-status.el" doom-private-dir))
  (load (concat doom-private-dir "plugins/harpoon.el"))
  (load (expand-file-name "plugins/library-fixes.el" doom-private-dir)))

(when (eq doom-theme 'doom-one)
  (custom-set-faces
   '(line-number ((t (:inherit default :foreground "gray40" :strike-through nil :underline nil :slant normal :weight normal))))))

(defun upgrade-doom-emacs-on-rails ()
  "Pull, Sync and upgrade el file"
  (interactive)
  (shell-command (concat "cd " doom-private-dir "; git pull; " doom-emacs-dir "bin/doom sync"))
  (recompile-doom-emacs)
  (message "Upgrade done!  Please restart your config"))

(defun recompile-doom-emacs ()
  "Pull, Sync and upgrade el file"
  (interactive)
  (shell-command "rm ~/.doom.d/doom-settings.el")
  (org-babel-load-file
   (expand-file-name "doom-settings.org" doom-private-dir))
  (message "Compilation done."))

(defun reload-user-settings ()
  "Pull, Sync and upgrade el file"
  (interactive)
  (load (expand-file-name "user-settings.el" doom-private-dir))
  (doom/reload-font)
  (doom/reload-theme))

(defun visit-config-utils ()
  "Visit the user-settings.el."
  (interactive)
  (find-file (concat doom-private-dir "user-settings.el"))
  (message "Welcome to your settings file!"))

(map! :leader "fm" 'visit-config-utils)

(when (not (file-exists-p "~/.pryrc")) (shell-command "cp ~/.doom.d/.pry-example ~/.pryrc"))
(if (not (file-exists-p "~/.irbrc")) (shell-command "cp ~/.doom.d/.irbrc-example ~/.irbrc"))

(after! google-translate
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))

  (map! :nvi "C-c ! t" 'google-translate-query-translate)
  (map! :nvi "C-c ! T" 'google-translate-query-translate-reverse)
  (map! :nvi "C-c t" 'google-translate-at-point)
  (map! :nvi "C-c T" 'google-translate-at-point-reverse)

  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "pt"))

(after! google-translate-default-ui
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130)))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(map! :leader "j c" 'harpoon-clear)
(map! :leader "j f" 'harpoon-toggle-file)
(map! :n "C-s" 'harpoon-add-file)
(map! :n "C-SPC" 'harpoon-toggle-quick-menu)
(map! :leader "1" 'harpoon-go-to-1)
(map! :leader "2" 'harpoon-go-to-2)
(map! :leader "3" 'harpoon-go-to-3)
(map! :leader "4" 'harpoon-go-to-4)
(map! :leader "5" 'harpoon-go-to-5)
(map! :leader "6" 'harpoon-go-to-6)
(map! :leader "7" 'harpoon-go-to-7)
(map! :leader "8" 'harpoon-go-to-8)
(map! :leader "9" 'harpoon-go-to-9)

(add-hook! 'ruby-mode-hook (sp-local-pair 'ruby-mode "{" "}" :actions '(wrap insert autoskip navigate) :unless '(sp-point-before-word-p sp-point-before-same-p) :post-handlers '(("||
[i]" "RET") ("| " "SPC"))))

(setq scroll-margin 3)

(map! "M-c" 'string-inflection-toggle)
(map! "M-S-c" 'string-inflection-cycle)

(after! web-mode
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(after! js2-mode
  (setq js-indent-level 2)
  (setq indent-tabs-mode nil))

(after! rjsx-mode
  (setq js-indent-level 2)
  (setq indent-tabs-mode nil))

(defun indent-whole-buffer ()
  "INDENT WHOLE BUFFER."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(map! :leader "-" #'indent-whole-buffer)

;; Save all
(defun save-all-buffers ()
  (interactive)
  (save-some-buffers 0))

(map! :n "ç" #'save-all-buffers)
(map! :n "\\" #'save-all-buffers)

;; Previous and next buffer
(map! :n "C-," #'previous-buffer)
(map! :n "C-;" #'next-buffer)

(map! :v "K" #'drag-stuff-up)
(map! :v "J" #'drag-stuff-down)

(map! "C-M-k" #'drag-stuff-up)
(map! "C-M-j" #'drag-stuff-down)

(map! :nv "0" #'doom/backward-to-bol-or-indent)
(map! :nv "-" #'end-of-line)

(map! :leader "k" #'kill-current-buffer)

(map! :nv "]g" #'git-gutter:next-hunk)
(map! :nv "[g" #'git-gutter:previous-hunk)

(map! :nv "M-s" #'evil-avy-goto-char-2)

(global-set-key (kbd "C-j") (kbd "C-M-n"))
(global-set-key (kbd "C-k") (kbd "C-M-p"))

(setq-default evil-escape-key-sequence "jj")
(setq-default evil-escape-delay 0.5)

(map! :after vterm :map vterm-mode-map :ni "C-l" #'vterm-clear)

(defun better-vterm-paste ()
  (interactive)
  (+vterm-send-string (substring-no-properties (current-kill 0)) nil))

(defun better-vterm-clean ()
  (interactive)
  (vterm-send-C-c)
  (evil-insert 1))

(map! :after vterm :map vterm-mode-map :n "P" #'better-vterm-paste-before)
(map! :after vterm :map vterm-mode-map :n "p" #'better-vterm-paste)
(map! :after vterm :map vterm-mode-map :i "C-v" #'better-vterm-paste)
(map! :after vterm :map vterm-mode-map :i "M-v" #'better-vterm-paste)
(map! :after vterm :map vterm-mode-map :n "C-p" #'vterm-send-M-p)
(map! :after vterm :mode vterm-mode :n "C-n" #'vterm-send-M-n)
(map! :after vterm :mode vterm-mode :n "C-c" #'better-vterm-clean)

(map! :leader "v" #'+vterm/toggle)

(defun +vterm-toggle--create-terms ()
  (+vterm/here nil)
  (+workspaces-add-current-buffer-h)
  (evil-insert 1)
  (evil-window-vsplit)
  (+vterm/here nil)
  (+workspaces-add-current-buffer-h)
  (evil-insert 1)
  (message "Terminals created.  Go back to your code with SPC TAB [ or M-1 to M-9. Switch between terminals with M-h and M-l"))

(defvar +vterm-layouts '() "Command to be executed on terminal 1")
(defvar +vterm-commands '() "Command to execute with SPC o t")

(defun +add-layout-to-term-list (command)
  "Add a layout to vterm"
  (push command +vterm-layouts))

(defun +add-command-to-term-list (command)
  "Execute the command with +vterm."
  (push command +vterm-commands))

(defun +vterm-execute-command-term ()
  (interactive)
  (let ((item (completing-read "Select command: " +vterm-commands)))
    (when (not (string= item ""))
      (let* ((item-to-run (assoc item +vterm-commands))
             (command (eval (cdr item-to-run))))
        (+vterm--create-term-with-command command item)))))

(defun +vterm--create-term-with-command (command buffer)
  "Create a vterm with specified command"
  (interactive)
  (if (member buffer (mapcar (lambda (x) (format "%s" x)) (buffer-list)))
      (switch-to-buffer buffer)
    (progn
      (+vterm/here nil)
      (+workspaces-add-current-buffer-h)
      (rename-buffer buffer t)
      (+vterm-send-string command t))))

(defun +vterm-switch-to-terminal ()
  "Go to vterm terminals."
  (interactive)
  (let* ((terminals
          (remove nil (mapcar
                       (lambda (buf)
                         (with-current-buffer buf (and (not (string-match-p ".*vterm-popup.*" (format "%s" buf))) (when (eq major-mode 'vterm-mode) buf))))
                       (buffer-list (current-buffer)))))
         (terminal-to-go (completing-read "Select the terminal: " (mapcar (lambda (x) (format "%s" x)) terminals))))
    (when (not (string= terminal-to-go ""))
      (switch-to-buffer terminal-to-go))))

(defun +vterm-send-selected-text-to-terminal ()
  (interactive)
  (call-interactively 'evil-yank)
  (let* ((terminals
          (remove nil (mapcar
                       (lambda (buf)
                         (with-current-buffer buf (when (eq major-mode 'vterm-mode) buf)))
                       (buffer-list (current-buffer)))))
         (terminal-to-go (completing-read "Select the terminal to send region: " (mapcar (lambda (x) (format "%s" x)) terminals))))

    (when (not (string= terminal-to-go ""))
      (switch-to-buffer terminal-to-go)
      (better-vterm-paste)
      (evil-insert 1))))

(map! :desc "Switch to Terminal" :n "SPC l" #'+vterm-switch-to-terminal)
(map! :desc "Send Text to Terminal" :v "SPC l" #'+vterm-send-selected-text-to-terminal)

(map! :leader "o t" #'+vterm-execute-command-term)

(defun +vterm-with-command-splitted (command-name commands)
  (interactive)
  (if (projectile-project-name)
      (+workspace-new (concat (projectile-project-name)" - " command-name " - C Terms"))
    (+workspace-new "Custom Terminals"))
  (+workspace/switch-to-final)
  (mapc (lambda (command)
          (+vterm/here nil)
          (+workspaces-add-current-buffer-h)
          (rename-buffer (concat command-name " - term") t)
          (when command
            (+vterm-send-string command t))
          (evil-insert 1)
          (unless (-contains? (last commands) command)
            (evil-window-vsplit))
          ) commands))

(defun +vterm-create-layout ()
  (interactive)
  (let* ((item (completing-read "Select a layout: " +vterm-layouts)))
    (when (not (string= item ""))
      (+vterm-with-command-splitted item (car (cdr (cdr (assoc item +vterm-layouts))))))))

(defun +vterm-send-string (string send-return)
  (mapc (lambda (c)
          (if (string= c "\n") (vterm-send-return)
            (pcase c
              (" " (vterm-send-space))
              (_ (vterm-send c)))))
        (s-split "" string t))
  (when send-return (vterm-send-return)))

(map! :leader "T" '+vterm-create-layout)

(after! vterm
  (set-popup-rule! "^\\*\\(vterm\\)?" :ttl nil :size 0.4))

(setq vterm-always-compile-module t)

(defun better-paste-after ()
  (interactive)
  (yank))

(map! :i "C-v" #'better-paste-after)

(map! :leader "e" #'+treemacs/toggle)
(map! :leader "E" #'treemacs-find-file)
(map! :map treemacs-mode-map "M-k" #'evil-window-up)
(map! :map treemacs-mode-map "M-j" #'evil-window-down)
(map! :map treemacs-mode-map "M-h" #'evil-window-left)
(map! :map treemacs-mode-map "M-l" #'evil-window-right)

(after! treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(map! :mode smerge-mode-map :leader "gdo" #'smerge-keep-other)
(map! :mode smerge-mode-map :leader "gdm" #'smerge-keep-mine)
(map! :mode smerge-mode-map :leader "gda" #'smerge-keep-all)
(map! :mode smerge-mode-map :leader "gdc" #'smerge-keep-current)

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

(map! :n "C-M-d" #'evil-multiedit-match-all)

(after! evil-multiedit
  (map! :map iedit-occurrence-keymap-default
        "M-D" nil))

(setq flycheck-yamllintrc ".yamllint.yml")

(defadvice find-file-noselect (around find-file-noselect-at-line
                                      (filename &optional nowarn rawfile wildcards)
                                      activate)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename))
           (buffer-name ad-do-it))
      (when line-number
        (with-current-buffer buffer-name
          (goto-char (point-min))
          (forward-line (1- line-number)))))))

(map! :leader "]" '+popup/raise)

(setq avy-single-candidate-jump t)

(setq evil-want-visual-char-semi-exclusive t)
(add-hook! 'evil-insert-state-exit-hook #'better-jumper-set-jump)

(map! :ni "M-k" #'evil-window-up)
(map! :ni "M-j" #'evil-window-down)
(map! :ni "M-h" #'evil-window-left)
(map! :ni "M-l" #'evil-window-right)

(map! "M-o" #'evil-window-next)
(map! :map vterm-mode-map :n "C-<SPC>" #'ace-window)

(setq evil-split-window-below t evil-vsplit-window-right t)

(map! :after web-mode :map web-mode-map :i "M-e" #'emmet-expand-yas)
(map! :after js2-mode :map rjsx-mode-map :i "M-e" #'emmet-expand-yas)

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(".*\\.html\\.erb$" . "html")))

(map! :after web-mode :map web-mode-map :i "M-e" #'emmet-expand-yas)
(map! :after js2-mode :map rjsx-mode-map :i "M-e" #'emmet-expand-yas)
(map! :after web-mode :map web-mode-map :nvi "C-j" #'web-mode-tag-next)
(map! :after web-mode :map web-mode-map :nvi "C-k" #'web-mode-tag-previous)

;; Fixing annoying lose of highlight
(after! web-mode
  (defun msc/save-and-revert-buffer ()
    (interactive)
    (call-interactively 'save-buffer)
    (msc/revert-buffer-noconfirm)))

(defun otavio/swap-arg-forward ()
  (interactive)
  (evil-exchange (nth 0 (evil-inner-arg)) (nth 1 (evil-inner-arg)))
  (evil-forward-arg 1)
  (evil-exchange (nth 0 (evil-inner-arg)) (nth 1 (evil-inner-arg))))

(defun otavio/swap-arg-backward ()
  (interactive)
  (evil-exchange (nth 0 (evil-inner-arg)) (nth 1 (evil-inner-arg)))
  (evil-backward-arg 1)
  (evil-exchange (nth 0 (evil-inner-arg)) (nth 1 (evil-inner-arg))))

(map! :n "C-l" #'otavio/swap-arg-forward)
(map! :n "C-h" #'otavio/swap-arg-backward)

(after! vertico
  (map! :map vertico-map "C-c C-o" 'embark-collect-snapshot))

;; Show path of file on SPC ,
(after! vertico
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*"))

(setq iedit-toggle-key-default nil)

(after! emmet-mode
  (setq emmet-expand-jsx-className? nil))

(defun update-yas-indentation ()
  (setq-local yas-indent-line 'fixed))

(defun set-emmet-class-name ()
  (setq-local emmet-expand-jsx-htmlFor? t)
  (setq-local emmet-expand-jsx-className? t))

(after! lsp-mode
  (setq lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr")))

(add-hook! 'rjsx-mode-hook 'set-emmet-class-name)
(add-hook! 'yaml-mode-hook 'update-yas-indentation)

(defun company-complete-if-selected ()
  (interactive)
  (if (eq company-selection nil) (newline-and-indent) (company-complete)))

(after! ruby-mode
  (defconst ruby-common-words '("deep_symbolize_keys" "deep_stringify_keys" "greater_than" "to_json" "valid?" "invalid?"
                                "greater_than_or_equal_to" "equal_to" "less_than" "less_than_or_equal_to"
                                "other_than" "any?" "assoc" "clear" "Time.zone.now" "Date.today" "present?" "blank?" "nil?"
                                "compact" "compact!" "compare_by_identity" "compare_by_identity?"
                                "deconstruct_keys" "default" "default=" "default_proc"
                                "default_proc=" "delete" "delete_if" "dig"
                                "each" "each_key" "each_pair" "each_value"
                                "empty?" "eql?" "except" "fetch"
                                "fetch_values" "filter" "filter!" "flatten"
                                "has_key?" "has_value?" "hash" "include?"
                                "initialize_copy" "inspect" "invert" "keep_if"
                                "key" "key?" "keys" "length"
                                "member?" "merge" "merge!" "rassoc"
                                "rehash" "reject" "reject!" "replace"
                                "select" "select!" "shift" "size"
                                "slice" "store" "to_a" "to_h"
                                "to_hash" "to_proc" "to_s" "transform_keys"
                                "transform_keys!" "transform_values" "transform_values!" "update"
                                "value?" "values" "values_at" "all?"
                                "append" "at" "bsearch" "bsearch_index"
                                "collect" "collect!" "combination" "concat"
                                "count" "cycle" "deconstruct" "delete_at"
                                "difference" "drop" "drop_while" "each_index"
                                "fill" "find_index" "first" "flatten!"
                                "index" "insert" "intersection" "join"
                                "last" "map" "map!" "max"
                                "min" "minmax" "none?" "old_to_s"
                                "one?" "pack" "permutation" "pop"
                                "prepend" "product" "push" "repeated_combination"
                                "repeated_permutation" "reverse" "reverse!" "reverse_each"
                                "rindex" "rotate" "rotate!" "sample"
                                "shuffle" "shuffle!" "slice!" "sort"
                                "sort!" "sort_by!" "sum" "take"
                                "take_while" "to_ary" "transpose" "union"
                                "uniq" "uniq!" "unshift" "zip"
                                "ascii_only?" "bytes" "bytesize" "byteslice"
                                "capitalize" "capitalize!" "casecmp" "casecmp?"
                                "center" "chars" "chomp" "chomp!"
                                "chop" "chop!" "chr" "codepoints"
                                "crypt" "delete!" "delete_prefix" "delete_prefix!"
                                "delete_suffix" "delete_suffix!" "downcase" "downcase!"
                                "dump" "each_byte" "each_char" "each_codepoint"
                                "each_grapheme_cluster" "each_line" "encode" "encode!"
                                "encoding" "end_with?" "force_encoding" "freeze"
                                "getbyte" "grapheme_clusters" "gsub" "gsub!"
                                "hex" "intern" "lines" "ljust"
                                "lstrip" "lstrip!" "match" "match?"
                                "next" "next!" "oct" "ord"
                                "partition" "rjust" "rpartition" "rstrip"
                                "rstrip!" "scan" "scrub" "scrub!"
                                "setbyte" "split" "squeeze" "squeeze!"
                                "start_with?" "strip" "strip!" "sub"
                                "sub!" "succ" "succ!" "swapcase"
                                "swapcase!" "to_c" "to_f" "to_i"
                                "to_r" "to_str" "to_sym" "tr"
                                "tr!" "tr_s" "tr_s!" "undump"
                                "unicode_normalize" "unicode_normalize!" "unicode_normalized?" "unpack"
                                "unpack1" "upcase" "upcase!" "upto"
                                "valid_encoding?" "ajd" "amjd" "asctime"
                                "ctime" "cwday" "cweek" "cwyear"
                                "day" "day_fraction" "downto" "england"
                                "friday?" "gregorian" "gregorian?" "httpdate"
                                "infinite?" "inspect_raw" "iso8601" "italy"
                                "jd" "jisx0301" "julian" "julian?"
                                "ld" "leap?" "marshal_dump_old" "mday"
                                "mjd" "mon" "monday?" "month"
                                "new_start" "next_day" "next_month" "next_year"
                                "nth_kday?" "prev_day" "prev_month" "prev_year"
                                "rfc2822" "rfc3339" "rfc822" "saturday?"
                                "start" "step" "strftime" "strftime('%Y-%m-%d')" "strftime('%d/$m/%Y')" "sunday?"
                                "thursday?" "to_date" "to_datetime" "to_time"
                                "tuesday?" "wday" "wednesday?" "xmlschema"
                                "acceptance" "validates_associated" "confirmation"
                                "exclusion" "format" "inclusion" "perform_later" "perform_now" "set" "perform"
                                "numericality: " "presence: true" "presence: " "absence" "uniqueness" "allow_nil" "allow_blank" "message"
                                "uniqueness: true" "uniqueness: " "allow_nil: true" "allow_nil: " "allow_blank: true" "allow_blank: " "message: " "on: "
                                "yday" "year" "optional: false" "optional: true" "errors.full_messages.to_sentence" "before_action" "before_action :" "skip_before_action :" "protect_from_forgery with: :" "rescue_from :" "with: "
                                "acts_like_date?"
                                "advance"
                                "ago"
                                "at_beginning_of_day"
                                "at_end_of_day"
                                "at_midday"
                                "at_middle_of_day"
                                "at_midnight"
                                "at_noon"
                                "beginning_of_day"
                                "beginning_of_week"
                                "compare_with_coercion"
                                "compare_without_coercion"
                                "current"
                                "default_inspect"
                                "end_of_day"
                                "find_beginning_of_week!"
                                "midday"
                                "middle_of_day"
                                "midnight"
                                "noon"
                                "readable_inspect"
                                "since"
                                "to_time"
                                "tomorrow"
                                "yesterday"
                                )
    )
  (defconst rspec-common-words '("actual"
                                 "actual_exists?"
                                 "add_should_and_should_not_to"
                                 "and_return"
                                 "allow"
                                 "aggregate_failures"
                                 "aggregation_block_label"
                                 "aggregation_metadata"
                                 "lias_matcher"
                                 "all"
                                 "all_exceptions"
                                 "and"
                                 "argument"
                                 "at_least"
                                 "at_most"
                                 "backtrace_formatter"
                                 "be"
                                 "be_a"
                                 "be_a_kind_of"
                                 "be_an_instance_of"
                                 "be_between"
                                 "be_falsey"
                                 "be_nil"
                                 "be_truthy"
                                 "be_within"
                                 "block_arg"
                                 "by"
                                 "by_at_least"
                                 "by_at_most"
                                 "captures"
                                 "chain"
                                 "change"
                                 "lear_generated_description"
                                 "color?"
                                 "onfiguration"
                                 "contain_exactly"
                                 "cover"
                                 "efault_should_host"
                                 "define"
                                 "efine_negated_matcher"
                                 "description"
                                 "description_of"
                                 "diffable"
                                 "diffable?"
                                 "isable_expect"
                                 "isable_should"
                                 "does_not_match?"
                                 "nable_expect"
                                 "nable_should"
                                 "end_with"
                                 "eq"
                                 "eql"
                                 "equal"
                                 "exactly"
                                 "exception_count_description"
                                 "exclusive"
                                 "exist"
                                 "expect"
                                 "xpect_enabled?"
                                 "expected"
                                 "expected_as_array"
                                 "expects_call_stack_jump?"
                                 "fail"
                                 "fail_including"
                                 "ail_with"
                                 "fail_with"
                                 "failure_message"
                                 "failure_message_for_should"
                                 "failure_message_for_should_not"
                                 "failure_message_when_negated"
                                 "failures"
                                 "or_many_matchers"
                                 "rom"
                                 "from"
                                 "enerated_description"
                                 "have_attributes"
                                 "include"
                                 "include_chain_clauses_in_custom_matcher_descriptions?"
                                 "inclusive"
                                 "indeterminate_actual_indexes"
                                 "indeterminate_expected_indexes"
                                 "initialize"
                                 "inspect"
                                 "ist"
                                 "match"
                                 "match_array"
                                 "match_for_should"
                                 "match_for_should_not"
                                 "match_unless_raises"
                                 "match_when_negated"
                                 "matcher_matches?"
                                 "matches?"
                                 "message"
                                 "message_with_diff"
                                 "method_missing"
                                 "name"
                                 "names"
                                 "not_to"
                                 "of"
                                 "on_potential_false_positives"
                                 "once"
                                 "or"
                                 "other_errors"
                                 "output"
                                 "percent_of"
                                 "raise_error"
                                 "rescued_exception"
                                 "respond_to"
                                 "respond_to?"
                                 "respond_to_missing?"
                                 "satisfy"
                                 "should"
                                 "hould_enabled?"
                                 "hould_enumerate?"
                                 "should_not"
                                 "plit_words"
                                 "start_with"
                                 "summary"
                                 "supports_block_expectations"
                                 "supports_block_expectations?"
                                 "urface_descriptions_in"
                                 "syntax"
                                 "syntax="
                                 "target"
                                 "thrice"
                                 "throw_symbol"
                                 "times"
                                 "to"
                                 "to_stderr"
                                 "to_stderr_from_any_process"
                                 "to_stdout"
                                 "to_stdout_from_any_process"
                                 "twice"
                                 "unmatched_actual_indexes"
                                 "unmatched_expected_indexes"
                                 "nreadable_io?"
                                 "valid_test?"
                                 "validity_message"
                                 "values_match?"
                                 "warn_about_potential_false_positives="
                                 "warn_about_potential_false_positives?"
                                 "arn_about_should!"
                                 "arn_about_should_unless_configured"
                                 "with"
                                 "with_any_keywords"
                                 "with_captures"
                                 "with_keywords"
                                 "with_message"
                                 "with_unlimited_arguments"
                                 "yield_control"
                                 "yield_successive_args"
                                 "yield_with_args"
                                 "yield_with_no_args"
                                 "instance_double"
                                 "be_present"
                                 "be_blank"
                                 "be_valid"
                                 "be_invalid"))

  (defun company-rspec-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))

    (cl-case command
      (interactive (company-begin-backend 'company-ruby-backend))
      (prefix (and (and (boundp 'rspec-mode) rspec-mode)
                   (company-grab-symbol)))

      (candidates
       (all-completions arg rspec-common-words))))

  (defun company-ruby-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))

    (cl-case command
      (interactive (company-begin-backend 'company-ruby-backend))
      (prefix (and (or (eq major-mode 'ruby-mode) (eq major-mode 'inf-ruby-mode))
                   (company-grab-symbol)))

      (candidates
       (all-completions arg ruby-common-words)))))

(add-hook 'ruby-mode-hook
          (lambda ()
            (setq-local +lsp-company-backends '(:separate company-capf company-dabbrev-code company-ruby-backend company-rspec-backend company-yasnippet))
            (setq-local company-transformers '(remove-company-duplicates))))

(after! company
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t)
  (setq company-dabbrev-code-everywhere nil)
  (setq company-dabbrev-code-other-buffers t)
  (setq company-idle-delay 0)

  (defun remove-company-duplicates (candidates)
    "Order the snippets / text depending of priority. CANDIDATES: Company candidates."
    (if (< (length candidates) 200)
        (let* ((new-list '()))
          (dolist (candidate candidates)
            (let* ((stripped-candidate (substring-no-properties candidate)))
              (if (and (not (string= (substring stripped-candidate 0 1) ":"))
                       (not (-contains? (mapcar 'substring-no-properties new-list) stripped-candidate))) (push candidate new-list))))
          (reverse new-list)) candidates)))

(after! company
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t)
  (setq company-dabbrev-code-everywhere nil)
  (setq company-dabbrev-code-other-buffers t)
  (setq company-idle-delay 0))

(defun better-dabbrev-expand ()
  (interactive)
  (call-interactively 'dabbrev-expand)
  (company-abort))

(defun call-real-ret ()
  (interactive)
  (when company-selection (company-abort))
  (funcall (key-binding (kbd "RET"))))

(map! :i "<C-return>" 'better-dabbrev-expand)
(map! :i "C-k" 'better-dabbrev-expand)
(map! :i "M-RET" 'call-real-ret)
(map! :i "TAB" 'yas-expand)

(defun better-yas-expand ()
  (interactive)
  (if yas--active-snippets (select-and-yas-next) (yas-expand)))

(map! :after company
      :map company-active-map
      "TAB" 'better-yas-expand
      "<tab>" #'better-yas-expand
      "M-e" #'emmet-expand-line
      "M-RET" #'call-real-ret
      "S-TAB" 'company-complete-selection
      "C-k" 'better-dabbrev-expand
      "<C-return>" 'better-dabbrev-expand)

(after! company
  (setq company-dabbrev-code-everywhere t)
  (set-company-backend! 'inf-ruby-mode '(:separate company-dabbrev-code company-capf company-ruby-backend)))

(after! yasnippet
  (defun select-and-yas-next ()
    (interactive)
    (if (eq company-selection nil)
        (yas-next-field)
      (progn (company-abort) (yas-next-field))))

  (defun select-and-yas-previous ()
    (interactive)
    (if (eq company-selection nil)
        (yas-prev-field)
      (progn (company-abort) (yas-prev-field))))

  (defun emmet-expand-line ()
    (interactive)
    (if (eq major-mode 'ruby-mode)
        (otavio/grb)
      (emmet-expand-yas)))

  (after! yasnippet
    (add-to-list 'yas-snippet-dirs (concat doom-private-dir "user-snippets/")))

  (map! :map yas-keymap
        "TAB" #'select-and-yas-next
        "S-TAB" #'select-and-yas-previous
        "C-d" #'yas-skip-and-clear-field
        "M-e" #'emmet-expand-line))

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

    (evil-local-set-key 'normal (kbd "C-l") #'inf-ruby-exit)
    (define-key evil-insert-state-local-map (kbd "C-l") #'inf-ruby-exit)

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

(after! lsp-mode
  (setq lsp-auto-guess-root t)
  (setq lsp-solargraph-formatting nil)
  (setq lsp-solargraph-symbols nil)
  (setq lsp-solargraph-folding nil))

(after! ruby-mode
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

  (map! :after ruby-mode :mode ruby-mode :localleader "ww" #'otavio/rails-reset-all)
  (map! :after ruby-mode :mode ruby-mode :localleader "wk" #'otavio/kill-ruby-instances))

(after! projectile
  (map! :leader "r" #'projectile-rails-command-map)

  (defun load-projectile-rails ()
    (require 'projectile-rails))

  (run-at-time 2 nil 'load-projectile-rails))

(after! which-key
  (push '((nil . "projectile-rails-\\(.+\\)") . (nil . "\\1"))
        which-key-replacement-alist))

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

;;; projectile-rails-remaps.el -*- lexical-binding: t; -*-

(after! projectile-rails
  (setq projectile-rails-expand-snippet-with-magic-comment t)

  (defun projectile-rails-find-admin ()
    "Find a model."
    (interactive)
    (projectile-rails-find-resource
     "admin: "
     '(("app/admin/" "\\(.+\\)\\.rb$"))
     "app/admin/${filename}.rb"))

  (defun projectile-rails-find-current-admin ()
    "Find a model for the current resource."
    (interactive)
    (projectile-rails-find-current-resource "app/admin/"
                                            "${singular}\\.rb$"
                                            'projectile-rails-find-admin))

  (defun projectile-rails-find-business-or-service ()
    "Find a service."
    (interactive)
    (if (file-exists-p (concat (projectile-project-root) "app/business"))
        (projectile-rails-find-resource
         "business: "
         '(("app/business/" "\\(.+\\)\\.rb$"))
         "app/business/${filename}.rb")
      (if (file-exists-p (concat (projectile-project-root) "app/services"))
          (projectile-rails-find-resource
           "service: "
           '(("app/services/" "\\(.+\\)\\.rb$"))
           "app/services/${filename}.rb"))))

  (defun projectile-rails-find-service ()
    "Find all in graphql."
    (interactive)
    (projectile-rails-find-resource
     "service: "
     '(("app/services/" "\\(.+\\)\\.rb$"))
     "app/services/${filename}.rb"))

  (defun otavio/go-to-latest-migration ()
    (interactive)
    (find-file (aj-fetch-latest (concat (doom-project-root) "db/migrate/"))))

  (defun aj-fetch-latest (path)
    (let ((e (f-entries path)))
      (car (sort e (lambda (a b)
                     (not (time-less-p (aj-mtime a)
                                       (aj-mtime b))))))))

  (defun aj-mtime (f) (let ((attrs (file-attributes f))) (nth 5 attrs)))

  (defun projectile-rails-find-graphql-all ()
    "Find all in graphql."
    (interactive)
    (projectile-rails-find-resource
     "graphql: "
     '(("app/graphql/" "\\(.+\\)\\.rb$"))
     "app/graphql/${filename}.rb"))

  (map! :leader "rd" #'otavio/go-to-latest-migration)
  (map! :leader "rt" #'projectile-rails-find-admin)
  (map! :leader "rT" #'projectile-rails-find-current-admin)
  (map! :leader "rs" #'projectile-rails-find-business-or-service)
  (map! :leader "rS" #'projectile-rails-find-service)
  (map! :leader "rq" #'projectile-rails-find-graphql-all))

(after! rspec-mode
  (set-popup-rule! "^\\*\\(rspec-\\)?compilation" :size 0.5 :ttl nil :select t))

(after! rspec-mode
  (map! :leader "t" #'rspec-mode-keymap)
  (map! :leader "tl" #'rspec-run-last-failed)
  (map! :leader "tg" #'rspec-run-git-diff-from-head)
  (map! :leader "tG" #'rspec-run-git-diff-from-master))

(after! ruby-mode
  (map! :mode ruby-mode :leader "a" 'goto-test)
  (map! :mode ruby-mode :leader "A" 'goto-test-and-vsplit))

(after! ruby-mode
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

;; make flycheck use bundle instead of rubocop latest version
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
  (map! :map ruby-mode-map
        "C-k" #'ruby-beginning-of-block
        "C-j" #'ruby-end-of-block))

(after! evil
  (define-key evil-normal-state-map (kbd "g S") #'multi-line)
  (define-key evil-normal-state-map (kbd "g J") #'multi-line-single-line))

(defvar debugger-command "require 'pry'; binding.pry")
(defvar pry-show-helper nil)

(defun otavio/remove-all-debuggers ()
  (interactive)
  (setq CURRENT_LINE (line-number-at-pos))
  (setq DELETATIONS 0)
  (goto-char (point-min))
  (while (search-forward debugger-command (point-max) t)
    (beginning-of-line)
    (kill-line 1)
    (setq DELETATIONS (1+ DELETATIONS)))
  (goto-char (point-min))
  (forward-line (- (1- CURRENT_LINE) DELETATIONS))
  (save-buffer))

(defun otavio/insert-debugger ()
  (interactive)
  (setq HELPER (if pry-show-helper " # next; step; break; break 14;break FooBar#func;break --help;" ""))
  (setq REAL_COMMAND (if (eq major-mode 'ruby-mode) (concat debugger-command HELPER) (concat "<% " debugger-command HELPER " %>")))
  (back-to-indentation)
  (newline-and-indent)
  (forward-line -1)
  (insert REAL_COMMAND)
  (indent-according-to-mode)
  (save-buffer))

(map! :after ruby-mode :mode ruby-mode :leader "d" 'otavio/insert-debugger)
(map! :after ruby-mode :mode ruby-mode :leader "D" 'otavio/remove-all-debuggers)
(map! :after web-mode :mode web-mode-map :leader "d" 'otavio/insert-debugger)
(map! :after web-mode :mode web-mode-map :leader "D" 'otavio/remove-all-debuggers)

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
  (map! :map ruby-mode-map :localleader "C" #'endless/ruby-copy-class-name)
  (map! :map ruby-mode-map :localleader "c" #'endless/ruby-copy-class-name))

(after! web-mode
  (define-key web-mode-map (kbd "C-c o") #'rails-routes-insert)
  (define-key web-mode-map (kbd "C-c C-o") #'rails-routes-insert-no-cache))

(after! ruby-mode
  (map! :mode ruby-mode "C-c o" #'rails-routes-insert)
  (map! :mode ruby-mode "C-c C-o" #'rails-routes-insert-no-cache))

(after! evil
  (define-key evil-normal-state-map (kbd "g a") #'rails-routes-jump)
  (define-key evil-visual-state-map (kbd "g a") #'rails-routes-jump))

;; On doom emacs
(after! ruby-mode
  (map! :mode ruby-mode :localleader "J" 'ruby-json-to-hash-parse-json) ;; Parse the json, SPC m J
  (map! :mode ruby-mode :localleader "j" 'ruby-json-to-hash-toggle-let)) ;; Create a let or send the let back to parent. SPC m j

(after! ruby-mode
  (map! :map ruby-mode-map "C-c i" 'rails-i18n-insert-with-cache) ;; Search with cache on ruby mode
  (map! :map ruby-mode-map "C-c C-i" 'rails-i18n-insert-no-cache) ;; Search refresh cache on ruby modee
  (map! :map web-mode-map "C-c i" 'rails-i18n-insert-with-cache) ;; Search with cache on web-mode
  (map! :map web-mode-map "C-c C-i" 'rails-i18n-insert-no-cache)) ;; Search refresh cache web-mode

(after! ruby-mode
  (define-key ruby-mode-map (kbd "C-c s") #'rails-http-statues-insert-symbol)
  (define-key ruby-mode-map (kbd "C-c S") #'rails-http-statues-insert-code))

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

  (map! :map ruby-mode-map :localleader "d" 'rubocop-toggle-at-point)
  (map! :mode ruby-mode-map :leader "=" #'rubocop-on-current-file))

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
    (setq CHANGED nil)
    (if (not CHANGED)
        (setq CHANGED (otavio/-swap-search-forward-swap-to-multiline " if ")))
    (if (not CHANGED)
        (setq CHANGED (otavio/-swap-search-forward-swap-to-multiline " unless ")))
    (if (not CHANGED)
        (setq CHANGED (otavio/-swap-search-forward-swap-to-singleline "if")))
    (if (not CHANGED)
        (setq CHANGED (otavio/-swap-search-forward-swap-to-singleline "unless")))
    (if (not CHANGED)
        (progn
          (forward-line -1)
          (beginning-of-line)
          (forward-word)))
    (if (not CHANGED)
        (setq CHANGED (otavio/-swap-search-forward-swap-to-singleline "if")))
    (if (not CHANGED)
        (setq CHANGED (otavio/-swap-search-forward-swap-to-singleline "unless")))
    (if (not CHANGED)
        (progn
          (forward-line -1)
          (beginning-of-line)
          (forward-word)))
    (if (not CHANGED)
        (setq CHANGED (otavio/-swap-search-forward-swap-to-singleline "if")))
    (if (not CHANGED)
        (setq CHANGED (otavio/-swap-search-forward-swap-to-singleline "unless"))))

  (map! :map ruby-mode-map :localleader "i" #'otavio/swap-if-unless-ruby))

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

  (map! :map ruby-mode-map :localleader "S" #'otavio/split-ruby-giant-string))

(after! ruby-mode
  (defun ruby-add-parameter--with-existing-parameters (args)
    (interactive)
    (forward-char -1)
    (insert ", " args))

  (defun ruby-add-parameter--without-existing-parameters (args)
    (interactive)
    (call-interactively 'end-of-line)
    (insert "(" args ")"))

  (defun ruby-add-parameter ()
    (interactive)
    (let (
          (args (read-string "Please enter the parameters that you want to add (separated by commma): "))
          )
      (when (not (string= args ""))
        (save-excursion
          (+evil/previous-beginning-of-method 1)
          (if (search-forward ")" (point-at-eol) t)
              (ruby-add-parameter--with-existing-parameters args)
            (ruby-add-parameter--without-existing-parameters args))))))

  (map! :mode ruby-mode :localleader "a" #'ruby-add-parameter))

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

  (map! :mode ruby-mode :localleader "m" #'ruby-extract-function))

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

  (map! :mode ruby-mode :localleader "n" #'ruby-new-method-from-symbol-at-point))

(after! ruby-mode
  (require 'ruby-refactor)
  (add-hook! 'ruby-mode-hook 'ruby-refactor-mode-launch))

(after! ruby-refactor
  (map! :mode ruby-mode :localleader "v" 'ruby-refactor-extract-local-variable)
  (map! :mode ruby-mode :localleader "V" 'ruby-refactor-extract-constant)

  (defun ruby-refactor-extract-local-variable ()
    "Extracts selected text to local variable"
    (interactive)
    (save-restriction
      (save-match-data
        (widen)
        (let* ((text-begin (region-beginning))
               (text-end (region-end))
               (text (ruby-refactor-trim-newline-endings (buffer-substring-no-properties text-begin text-end)))
               (variable-name (read-from-minibuffer "Variable name? ")))
          (delete-region text-begin text-end)
          (insert variable-name)
          (beginning-of-line)
          (open-line 1)
          (ruby-indent-line)
          (insert variable-name " = " text "\n")
          (search-forward variable-name)
          (backward-sexp)))))

  (defun ruby-refactor-extract-constant ()
    "Extracts selected text to a constant at the top of the current class or module"
    (interactive)
    (save-restriction
      (save-match-data
        (widen)
        (let* ((text-begin (region-beginning))
               (text-end (region-end))
               (text (ruby-refactor-trim-newline-endings (buffer-substring-no-properties text-begin text-end)))
               (constant-name (read-from-minibuffer "Constant name? ")))
          (delete-region text-begin text-end)
          (insert constant-name)
          (forward-line -1)
          (beginning-of-line)
          (evil-forward-word-begin)
          (let ((class-at-root (looking-at "class")) (first-character (substring text 0 1)))
            (ruby-refactor-goto-constant-insertion-point)
            (beginning-of-line)
            (if class-at-root
                (progn
                  (open-line 2)
                  (forward-line 1)))
            (ruby-indent-line)
            (if (or (string= "(" first-character)
                    (string= "[" first-character)
                    (string= "{" first-character)
                    (string= "\"" first-character)
                    (string= ":" first-character)
                    (string-match "^[a-zA-Z0-9_]+[_]*[a-zA-Z\w_]*$" text)
                    (string= "'" first-character))
                (insert constant-name " = " text ".freeze" "\n")
              (if (or (string-match "\\.\\." text)
                      (string-match "\\.\\.\\." text)
                      (string-match "\\+" text)
                      (string-match "\\-" text)
                      (not (string-match "^[a-zA-Z0-9_]+[_]*[a-zA-Z\w_]*$" text))
                      (string-match "\\*" text)
                      (string-match "\\*\\*" text))
                  (insert constant-name " = (" text ")" ".freeze" "\n") (insert constant-name " = " text ".freeze" "\n"))
              )

            (evil-indent-line (point-at-bol) (point-at-eol))
            (forward-line 1)
            (search-forward constant-name)
            (backward-sexp)))))))

(defvar ruby-rspec-describe-class "call")

(defun remove-accents (&optional @begin @end)
  "Remove accents in some letters and some
Change European language characters into equivalent ASCII ones, e.g. “café” ⇒ “cafe”.
When called interactively, work on current line or text selection.

URL `http://ergoemacs.org/emacs/emacs_zap_gremlins.html'
Version 2018-11-12"
  (interactive)
  (let (($charMap
         [
          ["ß" "ss"]
          ["á\\|à\\|â\\|ä\\|ā\\|ǎ\\|ã\\|å\\|ą\\|ă\\|ạ\\|ả\\|ả\\|ấ\\|ầ\\|ẩ\\|ẫ\\|ậ\\|ắ\\|ằ\\|ẳ\\|ặ" "a"]
          ["æ" "ae"]
          ["ç\\|č\\|ć" "c"]
          ["é\\|è\\|ê\\|ë\\|ē\\|ě\\|ę\\|ẹ\\|ẻ\\|ẽ\\|ế\\|ề\\|ể\\|ễ\\|ệ" "e"]
          ["í\\|ì\\|î\\|ï\\|ī\\|ǐ\\|ỉ\\|ị" "i"]
          ["ñ\\|ň\\|ń" "n"]
          ["ó\\|ò\\|ô\\|ö\\|õ\\|ǒ\\|ø\\|ō\\|ồ\\|ơ\\|ọ\\|ỏ\\|ố\\|ổ\\|ỗ\\|ộ\\|ớ\\|ờ\\|ở\\|ợ" "o"]
          ["ú\\|ù\\|û\\|ü\\|ū\\|ũ\\|ư\\|ụ\\|ủ\\|ứ\\|ừ\\|ử\\|ữ\\|ự"     "u"]
          ["ý\\|ÿ\\|ỳ\\|ỷ\\|ỹ"     "y"]
          ["þ" "th"]
          ["ď\\|ð\\|đ" "d"]
          ["ĩ" "i"]
          ["ľ\\|ĺ\\|ł" "l"]
          ["ř\\|ŕ" "r"]
          ["š\\|ś" "s"]
          ["ť" "t"]
          ["ž\\|ź\\|ż" "z"]
          [" " " "]       ; thin space etc
          ["–" "-"]       ; dash
          ["—\\|一" "--"] ; em dash etc
          ])
        $begin $end
        )
    (if (null @begin)
        (if (use-region-p)
            (setq $begin (region-beginning) $end (region-end))
          (setq $begin (line-beginning-position) $end (line-end-position)))
      (setq $begin @begin $end @end))
    (let ((case-fold-search t))
      (save-restriction
        (narrow-to-region $begin $end)
        (mapc
         (lambda ($pair)
           (goto-char (point-min))
           (while (search-forward-regexp (elt $pair 0) (point-max) t)
             (replace-match (elt $pair 1))))
         $charMap)))))

(defun remove--accents (@string)
  "Returns a new string. European language chars are changed ot ASCII ones e.g. “café” ⇒ “cafe”.
See `xah-asciify-text'
Version 2015-06-08"
  (with-temp-buffer
    (insert @string)
    (xah-asciify-text (point-min) (point-max))
    (buffer-string)))

(after! yasnippet
  (defun current-file-name-for-yas ()
    (interactive)
    (let* ((files (split-string buffer-file-name "/"))
           (file (nth (1- (length files)) files))
           (parsed (split-string file "\\."))
           (model (nth 0 parsed))
           )
      model)))

(use-package! kubernetes
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package! kubernetes-evil
  :config
  (map! :leader
        (:prefix "o"
         :desc "Kubernetes" "K" 'kubernetes-overview)))

(after! solidity-mode
  (require 'solidity-flycheck)
  (setq solidity-flycheck-solc-checker-active t)
  (set-company-backend! 'solidity-mode '(:separate company-solidity company-dabbrev-code)))

(use-package! lsp-tailwindcss
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (add-to-list 'lsp-language-id-configuration '(".*\\.erb$" . "html"))
  (setq lsp-tailwindcss-major-modes '(web-mode css-mode rjsx-mode typescript-tsx-mode)
        lsp-tailwindcss-emmet-completions (featurep 'emmet-mode)))

(set-docsets! '(web-mode css-mode rjsx-mode typescript-tsx-mode)
              :add "Tailwind_CSS")

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

(if (file-exists-p (expand-file-name "user-settings.el" doom-private-dir))
    (load (expand-file-name "user-settings.el" doom-private-dir))
  (progn
    (shell-command "cp ~/.doom.d/user-settings.example.el ~/.doom.d/user-settings.el")
    (load (expand-file-name "user-settings.el" doom-private-dir))))
