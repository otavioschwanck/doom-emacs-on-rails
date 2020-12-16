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

;; jj to escape evil, when you ever typed jj ?
(setq-default evil-escape-key-sequence "jj")
(setq-default evil-escape-delay 0.5)

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

(add-hook 'shell-mode-hook 'history-for-shell)

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

(add-hook 'kill-buffer-hook #'comint-write-input-ring)
(add-hook 'kill-emacs-hook
          (lambda ()
            (--each (buffer-list)
              (with-current-buffer it (comint-write-input-ring)))))
