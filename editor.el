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
(map! :after web-mode :map web-mode-map :i "C-e" #'emmet-expand)

(setq lsp-enable-file-watchers nil)

;; Don't want file watchers for JS
(defun set-file-watchers-h ()
  (setq-local lsp-enable-file-watchers (if (eq major-mode 'js2-mode) nil t)))

(add-hook! 'lsp-after-open-hook :append 'set-file-watchers-h)

(setq imported-rails-debugs nil)

;; debugger
(defun register-dap-to-project ()
  (dap-register-debug-template
   "Run Rails Debugger (port 3000)"
   (list :type "Ruby"
         :cwd (projectile-project-root)
         :request "launch"
         :program (concat (projectile-project-root) "bin/rails")
         :args ["server" "-p" "3000" "--pid" "tmp/pids/debugger.pid"]
         :name "Run Rails Debugger (port 3000)"))

  (dap-register-debug-template
   "Run Rails Debugger (port 3005)"
   (list :type "Ruby"
         :cwd (projectile-project-root)
         :request "launch"
         :program (concat (projectile-project-root) "bin/rails")
         :args ["server" "-p" "3005" "--pid" "tmp/pids/debugger_emacs.pid"]
         :name "Run Rails Debugger (port 3005)"))
  (setq imported-rails-debugs t))

(defun otavio/dap-debug ()
  (interactive)
  (if (not imported-rails-debugs) (register-dap-to-project))
  (call-interactively 'dap-debug))

;; Debug
(map! :leader "d d" 'dap-breakpoint-toggle)
(map! :leader "d D" 'otavio/dap-debug)
(map! :leader "d n" 'dap-step-in)
(map! :leader "d N" 'dap-next)
(map! :leader "d p" 'dap-step-out)
(map! :leader "d k" 'dap-disconnect)
(map! :leader "d w s" 'dap-ui-sessions)
(map! :leader "d w l" 'dap-ui-locals)
(map! :leader "d w e" 'dap-ui-expressions)
(map! :leader "d w b" 'dap-ui-breakpoints)
(map! :leader "d w r" 'dap-ui-repl)
(map! :leader "d r" 'dap-ui-repl)
(map! :leader "d c" 'dap-continue)
(map! :leader "d e" 'dap-eval-region)
(map! :leader "d E" 'dap-eval-thing-at-point)
(map! :leader "d K" 'dap-breakpoint-delete-all)

(setq dap-output-window-max-height 10)
