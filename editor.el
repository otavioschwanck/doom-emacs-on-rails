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
(map! :ieg "C-r" #'evil-paste-after)

(map! :leader "e" #'+treemacs/toggle)
(map! :leader "E" #'treemacs-find-file)

(map! :mode smerge-mode-map :leader "gdo" #'smerge-keep-other)
(map! :mode smerge-mode-map :leader "gdm" #'smerge-keep-mine)
(map! :mode smerge-mode-map :leader "gda" #'smerge-keep-all)
(map! :mode smerge-mode-map :leader "gdc" #'smerge-keep-current)

(add-hook! 'ruby-mode-hook (modify-syntax-entry ?_ "w"))
(add-hook! 'js2-mode-hook (modify-syntax-entry ?_ "w"))

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

(setq lsp-enable-file-watchers nil)

;; Don't want file watchers for JS
(defun set-file-watchers-h ()
  (setq-local lsp-enable-file-watchers (if (eq major-mode 'js2-mode) nil t)))

(add-hook! 'lsp-after-open-hook :append 'set-file-watchers-h)
(add-hook! 'lsp-completion-mode-hook 'init-company-dabbrev-code-h)
