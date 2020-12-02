;;; company.el --- Company                           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Doe

;; Keywords: autocomplete, company

(after! company
  (setq company-dabbrev-downcase 0)
  (setq company-show-numbers t)
  (setq company-idle-delay 0.04))

(defun init-company-dabbrev-code-h ()
    (when lsp-completion-mode
      (progn
        (setq BACKEND (if (eq major-mode 'ruby-mode) 'company-dabbrev-code 'company-capf))
        (setq-local company-backends (cons BACKEND company-backends)))))

(add-hook! 'lsp-completion-mode-hook 'init-company-dabbrev-code-h)
