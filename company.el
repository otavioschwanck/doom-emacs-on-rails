;;; company.el --- Company                           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Doe

;; Keywords: autocomplete, company

(after! company
  (setq company-dabbrev-downcase 0)
  (setq company-show-numbers t)
  (setq company-idle-delay 0))

(after! robe
  (set-company-backend! 'ruby-mode 'company-dabbrev-code 'company-capf 'company-yasnippet))

(add-hook! 'lsp-completion-mode-hook
  (defun init-company-dabbrev-code-h ()
    (when lsp-completion-mode
      (setq-local company-backends (cons 'company-dabbrev-code company-backends)))))
