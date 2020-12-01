;;; company.el --- Company                           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Doe

;; Keywords: autocomplete, company

(after! company
  (setq company-dabbrev-downcase 0)
  (setq company-show-numbers t)
  (setq company-idle-delay 0.01))

;; use C-p instead
(setq +lsp-company-backends '(company-capf :separate company-dabbrev-code))
