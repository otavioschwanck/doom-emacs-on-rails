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
  (if (eq major-mode 'ruby-mode) (progn (robe-start) (call-interactively 'company-robe))
    (when-let (backend (nth 1 company-backends))
      (company-begin-backend (nth 1 company-backends)))))

(map! :i "C-q" #'yas-expand)
(map! :i "C-p" #'dabbrev-expand)
(map! :i "C-S-p" #'+company/dabbrev)

(after! ruby-mode
  (map! :i :mode ruby-mode-map "C-l" #'current-mode-company-mode))

(map! :after company
      :map company-active-map
      "RET" #'newline-and-indent
      "<return>" #'newline-and-indent
      "<c-return>" #'newline-and-indent
      "<tab>" #'company-complete-selection
      "C-l" #'current-mode-company-mode
      "C-q" #'yas-expand)

(after! company
  (setq company-dabbrev-downcase 0)
  (setq company-show-numbers t)
  (setq company-idle-delay 0.04))

(defun yas-next-and-close-company ()
  (interactive)
  (company-abort)
  (yas-next-field))

(map! :after yasnippet
      :map yas-keymap
      "C-d" #'yas-skip-and-clear-field
      "C-e" #'emmet-expand
      "<tab>" #'company-complete-selection
      "C-q" 'yas-next-and-close-company)

(after! robe
  (set-company-backend! 'ruby-mode '(company-dabbrev-code :separate company-yasnippet) 'company-capf 'company-yasnippet))
