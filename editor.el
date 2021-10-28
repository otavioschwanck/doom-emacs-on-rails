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

;; Indent buffer
(defun indent-whole-buffer ()
  "INDENT WHOLE BUFFER."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; Fix monokai
(if (eq doom-theme 'doom-monokai-pro)
    (after! display-line-numbers
      (custom-set-faces!
        '(line-number :foreground "#6b6b6b")
        '(company-tooltip :foreground "#b8b8b8"))))

(defun history-for-shell ()
  (if (string-match-p "zsh\\'" shell-file-name)
      (progn
        (setq-local comint-input-ring-size 100000)
        (setq-local comint-input-ring-file-name "~/.zsh_history")
        (setq-local comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);")
        (comint-read-input-ring t))))

(defun history-for-inf-ruby ()
  (setq-local comint-input-ring-size 100000)
  (setq-local comint-input-ring-file-name "~/.pry_history")
  (comint-read-input-ring t))

(add-hook 'shell-mode-hook 'history-for-shell)
(add-hook 'inf-ruby-mode-hook 'history-for-inf-ruby)

(add-hook 'kill-buffer
          (lambda ()
            (if (eq major-mode 'inf-ruby-mode) (comint-write-input-ring))))

(add-hook 'kill-emacs-hook
          (lambda ()
            (--each (buffer-list)
              (with-current-buffer it (if (eq major-mode 'inf-ruby-mode) (comint-write-input-ring))))))

(setq kill-ring-max 200)

(defun current-file-name-for-yas ()
  (interactive)
  (let* ((files (split-string buffer-file-name "/"))
         (file (nth (1- (length files)) files))
         (parsed (split-string file "\\."))
         (model (nth 0 parsed))
         )
    model))

(after! counsel
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done))

(defun update-yas-indentation ()
  (setq-local yas-indent-line 'fixed))

(defun set-emmet-class-name ()
  (setq-local emmet-expand-jsx-htmlFor? t)
  (setq-local emmet-expand-jsx-className? t))

(add-hook! 'rjsx-mode-hook 'set-emmet-class-name)
(add-hook! 'yaml-mode-hook 'update-yas-indentation)

(set-popup-rule! "^\\*\\(vterm\\)?" :ttl nil)

(after! lsp-javascript
  (set-lsp-priority! 'ts-ls 1))

(setq +ivy-buffer-preview t)

;; Mac improvement
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'control)

(defvar rspec-run-git-diff-master-branch-name "master")

(after! rspec-mode
  (defun rspec-run-git-diff-from-master ()
    (interactive)
    (rspec-run-multiple-files (butlast (split-string (shell-command-to-string (concat "git diff " rspec-run-git-diff-master-branch-name " --name-only | grep _spec")) "\n"))))

  (defun rspec-run-git-diff-from-head ()
    (interactive)
    (rspec-run-multiple-files (butlast (split-string (shell-command-to-string "git diff HEAD --name-only | grep _spec") "\n")))))

(defun popserver-when-on-byebug (_SYMBOL NEWVAL _OPERATION _WHERE)
  (when (and (eq NEWVAL 0) (cl-search "projectile-rails" (buffer-name)))
    (progn (switch-to-buffer (buffer-name))
           (goto-char (point-max))
           (when (featurep 'evil)
             (evil-insert-state)))))

(add-variable-watcher 'inf-ruby-at-top-level-prompt-p 'popserver-when-on-byebug)

(after! vertico
  (map! :map vertico-map "C-c C-o" 'embark-collect-snapshot))

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*")
