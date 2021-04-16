;;; company.el --- Company                           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Doe

;; Keywords: autocomplete, company

;; C-p when company is active
(map! :after company
      :map company-active-map
      "C-S-p" #'+company/dabbrev
      "C-p" #'dabbrev-expand)

(map! :i "C-p" #'dabbrev-expand)
(map! :i "C-S-p" #'+company/dabbrev)

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun expand-snippet-or-next ()
  (interactive)
  (if (or (not yas/minor-mode)
      (null (do-yas-expand))
      (company-abort))
      (do-yas-expand)))

(after! ruby-mode
  (map! :i :mode ruby-mode-map "C-l" #'current-mode-company-mode))

(defun current-mode-company-mode ()
  (interactive)
  (if (eq major-mode 'ruby-mode) (progn (robe-start) (call-interactively 'company-robe))
    (when-let (backend (nth 1 company-backends))
      (company-begin-backend (nth 1 company-backends)))))

(map! :after company
      :map company-active-map
      "<C-SPC>" #'company-complete
      "<tab>" #'expand-snippet-or-next
      "C-q" #'company-complete
      "C-l" #'current-mode-company-mode)

(after! company
  (setq company-dabbrev-downcase 0)
  (setq company-show-numbers t)
  (setq company-idle-delay 0))

(defun yas-next-and-close-company ()
  (interactive)
  (if (company--active-p)
      (company-complete-selection))
  (yas-next-field))

(map! :after yasnippet
      :map yas-keymap
      "<tab>" #'yas-next-and-close-company
      "C-d" #'yas-skip-and-clear-field
      "C-e" #'emmet-expand)

(setq company-dabbrev-code-everywhere t)
(setq company-dabbrev-code-other-buffers t)

(after! robe
  (set-company-backend! 'ruby-mode '(company-dabbrev-code :separate company-yasnippet) 'company-robe 'company-yasnippet))

(setq truncate-lines t)

(after! inf-ruby
  (set-company-backend! 'inf-ruby-mode 'company-dabbrev-code 'company-capf))
