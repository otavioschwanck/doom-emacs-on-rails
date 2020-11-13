;;; company.el --- Company                           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Doe

;; Keywords: autocomplete, company

(after! company
  (setq company-dabbrev-downcase 0)
  (setq company-show-numbers t)
  (setq company-idle-delay 0))

(after! robe
  (set-company-backend! 'inf-ruby-mode 'company-tabnine 'company-dabbrev-code 'company-capf 'company-yasnippet)
  (set-company-backend! 'ruby-mode 'company-tabnine 'company-capf 'company-dabbrev-code 'company-yasnippet))

(after! js2-mode
  (set-company-backend! 'js2-mode 'company-tabnine 'company-tide 'company-dabbrev-code 'company-yasnippet))

(add-hook! 'lsp-completion-mode-hook
  (defun init-company-tabnine-h ()
    (when lsp-completion-mode
      (setq-local company-backends (cons 'company-tabnine company-backends)))))
