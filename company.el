;;; company.el --- Company                           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Doe

;; Keywords: autocomplete, company

;; C-p when company is active
(map! :after company
      :map company-active-map
      "C-S-p" #'+company/dabbrev
      "C-p" #'dabbrev-expand)

(defun current-mode-company-mode ()
  (interactive)
  (when-let (backend (nth 1 company-backends))
    (company-begin-backend (nth 1 company-backends))))

(map! :i "C-q" #'yas-expand)
(map! :i "C-p" #'dabbrev-expand)
(map! :i "C-S-p" #'+company/dabbrev)
(map! :i :mode ruby-mode-map "C-i" #'current-mode-company-mode)

(map! :after company
      :map company-active-map
      "RET" #'newline-and-indent
      "<c-return>" #'newline-and-indent
      "<tab>" #'company-complete-selection
      "C-i" #'current-mode-company-mode
      "C-q" #'yas-expand)

(after! company
  (setq company-dabbrev-downcase 0)
  (setq company-show-numbers t)
  (setq company-idle-delay 0.022))

(defun yas-next-and-close-company ()
  (interactive)
  (company-abort)
  (yas-next-field))

(map! :after yasnippet
      :map yas-keymap
      "C-d" #'yas-skip-and-clear-field
      "<tab>" #'company-complete-selection
      "C-q" 'yas-next-and-close-company)

(after! robe
  (set-company-backend! 'ruby-mode 'company-capf 'company-dabbrev-code 'company-yasnippet))

(after! inf-ruby
  (set-company-backend! 'inf-ruby-mode 'company-capf 'company-dabbrev-code 'company-dabbrev 'company-yasnippet))

;; use C-p instead
(setq +lsp-company-backends '(company-capf :separate company-yasnippet :with company-dabbrev))
