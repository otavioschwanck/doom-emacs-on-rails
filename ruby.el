;;; ruby.el --- Ruby config                          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Doe

;; Author: Otávio <otavioschwanck@gmail.com>
;; Keywords: ruby

;; Better C-j and C-k

;; Projectile globally with SPC r
(require 'projectile-rails)
(map! :leader "r" #'projectile-rails-command-map)

(after! which-key
  (push '((nil . "projectile-rails-\\(.+\\)") . (nil . "\\1"))
        which-key-replacement-alist))

(map! :map ruby-mode-map
      "C-k" #'ruby-beginning-of-block
      "C-j" #'ruby-end-of-block)

(setq debugger-command "byebug")
(setq pry-show-helper t)

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
  (forward-line (- (1- CURRENT_LINE) DELETATIONS)))

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

(defun otavio/-fix-forward-let-to-parent-replace-value (LET_NAME VALUE)
  (search-forward ": ")
  (setq START (point)) (search-forward LET_NAME) (kill-region START (point))
  (insert VALUE)
  t)

(defun otavio/-not-safe-backward-let-to-parent (LET_NAME VALUE)
  (if (search-backward (concat ": " LET_NAME) (point-min) t) (otavio/-fix-forward-let-to-parent-replace-value LET_NAME VALUE)))

(defun otavio/-not-safe-forward-let-to-parent (LET_NAME VALUE)
  (if (search-forward (concat ": " LET_NAME) (point-max) t) (otavio/-not-safe-backward-let-to-parent LET_NAME VALUE) nil))

(defun otavio/-fix-backward-let-to-parent (KEY_NAME LET_NAME VALUE)
  (if (search-backward (concat KEY_NAME ": " LET_NAME) (point-min) t)
      (progn
        (search-forward ": ")
        (setq START (point)) (search-forward LET_NAME) (kill-region START (point))
        (insert VALUE)
        t
        )))

(defun otavio/-fix-forward-let-to-parent (KEY_NAME LET_NAME VALUE)
  (if (search-forward (concat KEY_NAME ": " LET_NAME) (point-max) t) (otavio/-fix-backward-let-to-parent KEY_NAME LET_NAME VALUE) nil))

(defun otavio/return-let-to-parent ()
  (interactive)
  (setq INITIAL_LINE_NUM (line-number-at-pos))
  (beginning-of-line) (search-forward ":")
  (setq LET_NAME (let ((l (point)))
    (save-excursion
      (search-forward ")") (backward-char)
      (buffer-substring l (point)))))
  (setq CAMEL_LOWER_CASE (string-inflection-lower-camelcase-function LET_NAME))
  (setq CAMEL_CASE (string-inflection-camelcase-function LET_NAME))
  (setq UPPER_CASE (string-inflection-upcase-function LET_NAME))
  (search-forward "{ ")
  (setq VARIABLE_VALUE (let ((l (point)))
    (save-excursion
      (search-forward "}") (backward-char 2)
      (buffer-substring l (point)))))
  (message VARIABLE_VALUE)

  (setq FIXED (otavio/-fix-backward-let-to-parent LET_NAME LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)
  (if (not FIXED) (progn (setq FIXED (otavio/-fix-backward-let-to-parent CAMEL_LOWER_CASE LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)))
  (if (not FIXED) (progn (setq FIXED (otavio/-fix-backward-let-to-parent CAMEL_CASE LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)))
  (if (not FIXED) (progn (setq FIXED (otavio/-fix-backward-let-to-parent UPPER_CASE LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)))

  (if (not FIXED) (progn (setq FIXED (otavio/-fix-forward-let-to-parent LET_NAME LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)))
  (if (not FIXED) (progn (setq FIXED (otavio/-fix-forward-let-to-parent CAMEL_LOWER_CASE LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)))
  (if (not FIXED) (progn (setq FIXED (otavio/-fix-forward-let-to-parent CAMEL_CASE LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)))
  (if (not FIXED) (progn (setq FIXED (otavio/-fix-forward-let-to-parent UPPER_CASE LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)))

  (if (not FIXED)
      (progn
        (setq CAN_CONTINUE (y-or-n-p "Not found match of key: value in any JS pattern.  Want to search with only the let value ? Emacs will replace the first occurrence. (Recommended)"))
        (if CAN_CONTINUE (progn
                           (if (not FIXED) (progn (setq FIXED (otavio/-not-safe-backward-let-to-parent LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)))
                           (if (not FIXED) (progn (setq FIXED (otavio/-not-safe-backward-let-to-parent LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)))
                           (if (not FIXED) (progn (setq FIXED (otavio/-not-safe-backward-let-to-parent LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)))
                           (if (not FIXED) (progn (setq FIXED (otavio/-not-safe-backward-let-to-parent LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)))

                           (if (not FIXED) (progn (setq FIXED (otavio/-not-safe-forward-let-to-parent LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)))
                           (if (not FIXED) (progn (setq FIXED (otavio/-not-safe-forward-let-to-parent LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)))
                           (if (not FIXED) (progn (setq FIXED (otavio/-not-safe-forward-let-to-parent LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)))
                           (if (not FIXED) (progn (setq FIXED (otavio/-not-safe-forward-let-to-parent LET_NAME VARIABLE_VALUE)) (goto-line INITIAL_LINE_NUM)))
                           ))))

  (if FIXED (progn (kill-line 1) (message "Changed successfully.")) (message "Not found.")))

;; requires string-inflection package
(defun otavio/parse-json-to-ruby ()
  (interactive)
  (setq NO_UNDERSCORE_KEYS (y-or-n-p "Do you want to not underscore the keys (recommended for spec)"))
  (setq CREATE_LETS (y-or-n-p "Do you want to create the lets? if not, the object values will be keeped (recommend for spec)"))

  (if (not (eq ?{ (char-after)))
      (search-backward "{"))

  (condition-case err (json-read-object) (error
                                          (progn
                                            (if (cl-search "json-string-format" (format "%S" err))
                                                (progn
                                                  (message "JSON is invalid, trying to fix it...")
                                                  (search-backward ",") (delete-char 1))))))



  (if (not (eq ?{ (char-after)))
      (search-backward "{"))

  (setq JSON_NOT_PARSED (let ((l (point)))
    (save-excursion
      (setq JSON_OBJECT (json-read-object))
      (buffer-substring l (point)))))
  (search-backward JSON_NOT_PARSED (point-min) t)
  (setq START (point))
  (search-forward JSON_NOT_PARSED (point-max) t)
  (setq END (point))
  (kill-region START END)
  (insert "let(:) do") (indent-according-to-mode) (newline-and-indent)
  (insert "{") (newline-and-indent)
  (mapc (lambda (x) (insert (if (not NO_UNDERSCORE_KEYS) (string-inflection-underscore-function (symbol-name (car x))) (symbol-name (car x))))
          (insert ": ")
          (if CREATE_LETS
              (progn
                (insert (string-inflection-underscore-function (symbol-name (car x)))))
            (insert (format "%S" (cdr x))))
           (insert ",") (newline-and-indent))
        JSON_OBJECT)
  (search-backward ",") (delete-char 1) (newline-and-indent)
  (insert "}") (newline-and-indent) (insert "end") (newline-and-indent)
  (if CREATE_LETS
      (progn
        (newline-and-indent)
        (mapc (lambda (x) (insert "let(:" (string-inflection-underscore-function (symbol-name (car x))) ") { " (format "%S" (cdr x)) " }") (newline-and-indent)) JSON_OBJECT)
        ))
  (search-backward "let(:)") (search-forward ":")
  (if (featurep 'evil) (evil-insert 1)))

(use-package! string-inflection)

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
  (setq-default flycheck-disabled-checkers '(ruby-reek lsp ruby-rubylint)))

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

(map! :after web-mode :mode web-mode-map :leader "d" 'otavio/insert-debugger)
(map! :after web-mode :mode web-mode-map :leader "D" 'otavio/remove-all-debuggers)

(after! ruby-mode
  (map! :i :mode ruby-mode-map "<C-M-return>" #'otavio/grb)
  (map! :after ruby-mode :map ruby-mode-map :i "C-e" #'otavio/grb)
  (map! :map ruby-mode-map :localleader "L" 'otavio/parse-json-to-ruby)
  (map! :map ruby-mode-map :localleader "l" 'otavio/return-let-to-parent)
  (map! :map ruby-mode-map :localleader "i" 'otavio/swap-if-unless-ruby)
  (map! :map ruby-mode-map :localleader "S" 'otavio/split-ruby-giant-string)
  (map! :map ruby-mode-map :localleader "B" 'ruby-toggle-block)
  (map! :mode ruby-mode-map :leader "d" 'otavio/insert-debugger)
  (map! :mode ruby-mode-map :leader "D" 'otavio/remove-all-debuggers)
  ;; Better C-j and C-k
  (map! :map ruby-mode-map
        "C-k" #'ruby-beginning-of-block
        "C-j" #'ruby-end-of-block)
  (map! :mode ruby-mode-map :leader "=" #'rubocop-on-current-file)))

(add-hook 'ruby-mode-hook
  (lambda ()
    (setq-local flycheck-command-wrapper-function
                (lambda (command) (append '("bundle" "exec") command)))))
