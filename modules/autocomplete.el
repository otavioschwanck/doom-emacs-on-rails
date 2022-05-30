(add-hook 'ruby-mode-hook
          (lambda ()
            (setq-local +lsp-company-backends '(:separate company-capf company-dabbrev-code company-ruby-backend company-rspec-backend company-yasnippet))
            (setq-local company-transformers '(remove-company-duplicates))))

(after! company
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t)
  (setq company-idle-delay 0)

  (defun remove-company-duplicates (candidates)
    "Order the snippets / text depending of priority. CANDIDATES: Company candidates."
    (if (< (length candidates) 200)
        (let* ((case-fold-search nil)
               (word (word-at-point t))
               (new-list '()))
          (dolist (candidate candidates)
            (let* ((stripped-candidate (substring-no-properties candidate)))
              (if (and (string-match-p word candidate)
                       (not (string= (substring stripped-candidate 0 1) ":"))
                       (not (-contains? (mapcar 'substring-no-properties new-list) stripped-candidate))) (push candidate new-list))))
          (reverse new-list)) candidates)))

(defun better-dabbrev-expand ()
  (interactive)
  (call-interactively 'dabbrev-expand)
  (company-abort))

(defun call-real-ret ()
  (interactive)
  (when company-selection (company-abort))
  (funcall (key-binding (kbd "RET"))))

(map! :ig "C-o" 'better-dabbrev-expand)

(map! :i "S-<return>" 'call-real-ret)
(map! :i "C-j" 'yas-expand)
(map! :i "M-e" 'better-emmet-expand)

(after! company
  (setq company-dabbrev-code-everywhere t)
  (set-company-backend! 'inf-ruby-mode '(:separate company-dabbrev-code company-capf company-ruby-backend))

  (map! :after company
        :map company-active-map
        "M-e" #'better-emmet-expand
        "TAB" #'company-select-next
        "<tab>" #'company-select-next
        "S-TAB" #'company-select-previous
        "<backtab>" #'company-select-previous
        "M-RET" #'call-real-ret
        "C-j" 'yas-expand
        "C-o" 'better-dabbrev-expand
        "<C-return>" 'better-dabbrev-expand))

(after! yasnippet
  (defun better-emmet-expand ()
    (interactive)
    (if (eq major-mode 'ruby-mode)
        (otavio/grb)
      (emmet-expand-yas)))

  (map! :map yas-keymap
        "C-j" #'yas-next-field
        "C-k" #'yas-prev-field
        "C-d" #'yas-skip-and-clear-field
        "M-e" #'better-emmet-expand))

(after! company
  (setq company-dabbrev-downcase nil)
  (setq company-show-quick-access t)
  (setq company-idle-delay 0))

(after! yasnippet
    (add-to-list 'yas-snippet-dirs (concat doom-private-dir "user/snippets/"))
    (yas-load-directory (concat doom-private-dir "snippets/"))
    (yas-load-directory (concat doom-private-dir "user/snippets/")))
