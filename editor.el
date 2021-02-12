;;; editor.el --- Base Editor Config                 -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Doe

;; Author: Otávio Schwanck dos Santos <otavioshwanck@gmail.com>
;; Keywords: Editor

;; Save all
(defun save-all-buffers ()
  (interactive)
  (save-some-buffers 0))

(map! :n "ç" #'save-all-buffers)

;; Search with tab is easier on code
(map! :nv "<tab>" #'evil-ex-search-forward)
(map! :nv "<C-tab>" #'evil-ex-search-backward)

;; Previous and next buffer
(map! :n "C-," #'previous-buffer)
(map! :n "C-;" #'next-buffer)

;; Drag stuff
(map! :v "K" #'drag-stuff-up)
(map! :v "J" #'drag-stuff-down)
(map! :niv "C-M-h" #'drag-stuff-left)
(map! :niv "C-M-l" #'drag-stuff-right)
(map! :v "C-t" #'transpose-mark)

;; Another shortcut for begin and end of line
(map! :nv "0" #'doom/backward-to-bol-or-indent)
(map! :nv "-" #'end-of-line)

;; Better way to kill current buffer
(map! :leader "k" #'kill-current-buffer)

;; Navigate between git hunks
(map! :nv "]g" #'git-gutter:next-hunk)
(map! :nv "[g" #'git-gutter:previous-hunk)

;; Search with avy
(map! :nv "C-s" #'evil-avy-goto-char-2)

;; Shortcut for the emacs C-M-j and C-M-k
(global-set-key (kbd "C-j") (kbd "C-M-n"))
(global-set-key (kbd "C-k") (kbd "C-M-p"))

;; Indent 2 spaces
(after! web-mode
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(after! js2-mode
  (setq js-indent-level 2)
  (setq indent-tabs-mode nil))

;; jj to escape evil, when you ever typed jj ?
(setq-default evil-escape-key-sequence "jj")
(setq-default evil-escape-delay 0.5)

;; Indent buffer
(defun indent-whole-buffer ()
  "INDENT WHOLE BUFFER."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(map! :leader "-" #'indent-whole-buffer)

;; Clear terminal
(map! :mode shell-mode-map :leader "l" 'comint-clear-buffer)

;; Open Terminal
(map! :leader "v" #'projectile-run-shell)

;; Toggle truncate lines
(map! :leader "t t" #'toggle-truncate-lines)

;; Paste on insert mode
(map! :ieg "C-v" #'evil-paste-after)
(map! :ieg "C-V" #'evil-paste-before)

(map! :leader "e" #'+treemacs/toggle)
(map! :leader "E" #'treemacs-find-file)

(map! :mode smerge-mode-map :leader "gdo" #'smerge-keep-other)
(map! :mode smerge-mode-map :leader "gdm" #'smerge-keep-mine)
(map! :mode smerge-mode-map :leader "gda" #'smerge-keep-all)
(map! :mode smerge-mode-map :leader "gdc" #'smerge-keep-current)

;; New window command
(map! :ni "M-k" #'evil-window-up)
(map! :ni "M-j" #'evil-window-down)
(map! :ni "M-h" #'evil-window-left)
(map! :ni "M-l" #'evil-window-right)
(map! :ni "C-M-w" #'evil-window-next)

(after! treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

;; C-w C-w evil next > other-window
(map! :map evil-window-map "C-w" #'evil-window-next)
(map! "C-<SPC>" #'evil-window-next)
(map! :after web-mode :map web-mode-map :i "C-e" #'emmet-expand-yas)
(map! :after js2-mode :map rjsx-mode-map :i "C-e" #'emmet-expand-yas)
(map! :after web-mode :map web-mode-map :nvi "C-j" #'web-mode-tag-next)
(map! :after web-mode :map web-mode-map :nvi "C-k" #'web-mode-tag-previous)

(setq lsp-enable-file-watchers nil)

;; Don't want file watchers for JS
(defun set-file-watchers-h ()
  (setq-local lsp-enable-file-watchers (if (eq major-mode 'js2-mode) nil t)))

(add-hook! 'lsp-after-open-hook :append 'set-file-watchers-h)

;; Fix monokai
(if (eq doom-theme 'doom-monokai-pro)
    (after! display-line-numbers
      (custom-set-faces!
        '(line-number :foreground "#6b6b6b")
        '(font-lock-variable-name-face :foreground "#FB996C")
        '(company-tooltip :foreground "#b8b8b8"))))

(defun history-for-shell ()
  (if (string-match-p "zsh\\'" shell-file-name)
      (progn
        (setq-local comint-input-ring-size 10000)
        (setq-local comint-input-ring-file-name "~/.zsh_history")
        (setq-local comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);")
        (comint-read-input-ring t))))

(add-hook 'shell-mode-hook 'history-for-shell)
(setq uniquify-buffer-name-style 'forward)

(defun history-for-inf-ruby ()
  (setq-local comint-input-ring-file-name "~/.irb_history")
  (setq-local comint-input-ring-size 1000)
  (toggle-truncate-lines)
  (comint-read-input-ring t))

(after! inf-ruby
  (defun run-ruby-or-pop-to-buffer (command &optional name buffer)
    (if (not (and buffer
                  (comint-check-proc buffer)))
        (run-ruby-new command name)
      (pop-to-buffer buffer))))

(add-hook 'inf-ruby-mode-hook 'history-for-inf-ruby)

(setq kill-ring-max 200)

(after! counsel
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done))

(defun update-yas-indentation ()
  (setq-local yas-indent-line 'fixed))

(defun set-emmet-class-name ()
  (setq-local emmet-expand-jsx-htmlFor? t)
  (setq-local emmet-expand-jsx-className? t))

(add-hook! 'rjsx-mode-hook 'set-emmet-class-name)
(add-hook! 'yaml-mode-hook 'update-yas-indentation)

(set-popup-rule! "^\\*\\(shell\\)?" :ttl nil)

(after! lsp-javascript
  (set-lsp-priority! 'ts-ls 1))

(setq +ivy-buffer-preview t)

(after! treemacs
  (setq doom-themes-treemacs-theme 'colors))
