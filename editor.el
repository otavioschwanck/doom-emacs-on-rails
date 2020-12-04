;;; editor.el --- Base Editor Config                 -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Doe

;; Author: Otávio Schwanck dos Santos <otavioshwanck@gmail.com>
;; Keywords: Editor

;; Save all
(defun save-all-buffers ()
  (interactive)
  (save-some-buffers 0))

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

(setq imported-rails-debugs nil)

;; debugger
(defun register-dap-to-project ()
  (dap-register-debug-template
   "Run Rails Debugger (port 3000)"
   (list :type "Ruby"
         :cwd (projectile-project-root)
         :request "launch"
         :program (concat (projectile-project-root) "bin/rails")
         :args ["server" "-p" "3005" "--pid" "tmp/pids/debugger.pid"]
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

(setq dap-output-window-max-height 3)
(setq dap-output-window-min-height 2)
