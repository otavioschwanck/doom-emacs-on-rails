;;; company.el --- Company                           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Doe

;; Keywords: autocomplete, company

;; C-p when company is active
(map! :after company
      :map company-active-map
      "C-S-p" #'+company/dabbrev
      "C-p" #'dabbrev-expand)

(map! :i "<C-return>" #'yas-expand)
(map! :i "C-p" #'dabbrev-expand)
(map! :i "C-S-p" #'+company/dabbrev)
(map! :i "C-i" #'company-capf)

(map! :after company
      :map company-active-map
      "<tab>" #'company-complete-selection
      "RET" nil
      "<return>" nil
      "C-i" #'company-capf
      "<C-return>" #'yas-expand)

(after! company
  (setq company-dabbrev-downcase 0)
  (setq company-show-numbers t)
  (setq company-idle-delay 0))

(defun yas-next-and-close-company ()
  (interactive)
  (company-abort)
  (yas-next-field))

(map! :after yasnippet
      :map yas-keymap
      "<C-S-return>" 'yas-prev-field
      "<C-return>" 'yas-next-and-close-company)
