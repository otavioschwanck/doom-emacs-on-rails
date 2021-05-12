;;; editor.el --- Base Editor Config                 -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Doe

;; Author: Otávio Schwanck dos Santos <otavioshwanck@gmail.com>
;; Keywords: Editor

;; Save all
(defun save-all-buffers ()
  (interactive)
  (save-some-buffers 0))

(map! :n "ç" #'save-all-buffers)

(map! :after vterm
      :map vterm-mode-map
      :ni "C-l" #'vterm-clear)

;; Search with tab is easier on code
(map! :nv "<tab>" #'evil-ex-search-forward)
(map! :nv "<C-tab>" #'evil-ex-search-backward)

;; Previous and next buffer
(map! :n "C-," #'previous-buffer)
(map! :n "C-;" #'next-buffer)

;; Drag stuff
(map! :v "K" #'drag-stuff-up)
(map! :v "J" #'drag-stuff-down)
(map! :niv "C-M-h" #'drag-stuff-left)
(map! :niv "C-M-l" #'drag-stuff-right)
(map! :v "C-t" #'transpose-mark)

;; Another shortcut for begin and end of line
(map! :nv "0" #'doom/backward-to-bol-or-indent)
(map! :nv "-" #'end-of-line)

;; Better way to kill current buffer
(map! :leader "k" #'kill-current-buffer)

;; Navigate between git hunks
(map! :nv "]g" #'git-gutter:next-hunk)
(map! :nv "[g" #'git-gutter:previous-hunk)

;; Search with avy
(map! :nv "C-s" #'evil-avy-goto-char-2)

;; Shortcut for the emacs C-M-j and C-M-k
(global-set-key (kbd "C-j") (kbd "C-M-n"))
(global-set-key (kbd "C-k") (kbd "C-M-p"))

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

(map! :leader "-" #'indent-whole-buffer)

;; Clear terminal
(map! :mode shell-mode-map :leader "l" 'comint-clear-buffer)

;; Open Terminal
(map! :leader "v" #'projectile-run-vterm)

;; Toggle truncate lines
(map! :leader "t t" #'toggle-truncate-lines)

;; Paste on insert mode
(map! :ieg "C-v" #'evil-paste-after)
(map! :ieg "C-V" #'evil-paste-before)
(map! :iego "M-v" #'evil-paste-after)

(map! :leader "e" #'+treemacs/toggle)
(map! :leader "E" #'treemacs-find-file)

(map! :mode smerge-mode-map :leader "gdo" #'smerge-keep-other)
(map! :mode smerge-mode-map :leader "gdm" #'smerge-keep-mine)
(map! :mode smerge-mode-map :leader "gda" #'smerge-keep-all)
(map! :mode smerge-mode-map :leader "gdc" #'smerge-keep-current)

;; New window command
(map! :ni "M-k" #'evil-window-up)
(map! :ni "M-j" #'evil-window-down)
(map! :ni "M-h" #'evil-window-left)
(map! :ni "M-l" #'evil-window-right)
(map! :ni "C-M-w" #'evil-window-next)

(after! treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

;; C-w C-w evil next > other-window
(map! :map evil-window-map "C-w" #'evil-window-next)
(map! "C-<SPC>" #'evil-window-next)
(map! :map vterm-mode-map :n "C-<SPC>" #'evil-window-next)
(map! "M-o" #'evil-window-next)
(map! :map vterm-mode-map "M-o" #'evil-window-next)
(map! :after web-mode :map web-mode-map :i "C-e" #'emmet-expand-yas)
(map! :after js2-mode :map rjsx-mode-map :i "C-e" #'emmet-expand-yas)
(map! :after web-mode :map web-mode-map :nvi "C-j" #'web-mode-tag-next)
(map! :after web-mode :map web-mode-map :nvi "C-k" #'web-mode-tag-previous)
(map! :after web-mode :map web-mode-map :i "<tab>" #'+web/indent-or-yas-or-emmet-expand)

(setq lsp-enable-file-watchers nil)

;; Don't want file watchers for JS
(defun set-file-watchers-h ()
  (setq-local lsp-enable-file-watchers (if (eq major-mode 'js2-mode) nil t)))

(add-hook! 'lsp-after-open-hook :append 'set-file-watchers-h)

;; Fix monokai
(if (eq doom-theme 'doom-monokai-pro)
    (after! display-line-numbers
      (custom-set-faces!
        '(line-number :foreground "#6b6b6b")
        '(font-lock-variable-name-face :foreground "#FB996C")
        '(company-tooltip :foreground "#b8b8b8"))))

(setq uniquify-buffer-name-style 'forward)

(defun history-for-inf-ruby ()
  (setq-local comint-input-ring-file-name "~/.irb_history")
  (setq-local comint-input-ring-size 1000)
  (toggle-truncate-lines)
  (comint-read-input-ring t))

(after! inf-ruby
  (defun run-ruby-or-pop-to-buffer (command &optional name buffer)
    (if (not (and buffer
                  (comint-check-proc buffer)))
        (run-ruby-new command name)
      (pop-to-buffer buffer))))

(add-hook 'inf-ruby-mode-hook 'history-for-inf-ruby)

(setq kill-ring-max 200)

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

(after! web-mode
  (defun msc/save-and-revert-buffer ()
    (interactive)
    (call-interactively 'save-buffer)
    (msc/revert-buffer-noconfirm))

  (map! :mode web-mode-map :leader "j" 'msc/save-and-revert-buffer))

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

(setq vterm-always-compile-module t)

(setq company-dabbrev-code-everywhere t)
(setq company-dabbrev-code-other-buffers t)
(setq company-dabbrev-code-modes t)

(setq evil-split-window-below t evil-vsplit-window-right t)

(defun popserver-when-on-byebug (_SYMBOL NEWVAL _OPERATION _WHERE)
  (when (and (eq NEWVAL 0) (cl-search "projectile-rails" (buffer-name)))
    (progn (switch-to-buffer (buffer-name))
           (goto-char (point-max))
           (when (featurep 'evil)
             (evil-insert-state)))))

(add-variable-watcher 'inf-ruby-at-top-level-prompt-p 'popserver-when-on-byebug)

(map! :ieg "M-w" (cmd! (insert "?")))
(map! :ieg "M-;" (cmd! (insert "?")))
(map! :ieg "M-}" (cmd! (insert "|")))
(map! :ieg "M-]" (cmd! (insert "\\")))
(map! :ieg "M-Q" (cmd! (insert "\\")))

(map! :leader "n" #'tab-new)
(map! :leader "N" #'tab-list)

(define-key evil-normal-state-map (kbd "g t") #'tab-next)

(setq-hook! '(ruby-mode-hook js2-mode-hook) fill-column 125)
(setq truncate-lines nil)

(after! treemacs
  (map! :map treemacs-mode-map "M-l" #'evil-window-right))

(defun remove-accents (&optional @begin @end)
  "Remove accents in some letters and some
Change European language characters into equivalent ASCII ones, e.g. “café” ⇒ “cafe”.
When called interactively, work on current line or text selection.

URL `http://ergoemacs.org/emacs/emacs_zap_gremlins.html'
Version 2018-11-12"
  (interactive)
  (let (($charMap
         [
          ["ß" "ss"]
          ["á\\|à\\|â\\|ä\\|ā\\|ǎ\\|ã\\|å\\|ą\\|ă\\|ạ\\|ả\\|ả\\|ấ\\|ầ\\|ẩ\\|ẫ\\|ậ\\|ắ\\|ằ\\|ẳ\\|ặ" "a"]
          ["æ" "ae"]
          ["ç\\|č\\|ć" "c"]
          ["é\\|è\\|ê\\|ë\\|ē\\|ě\\|ę\\|ẹ\\|ẻ\\|ẽ\\|ế\\|ề\\|ể\\|ễ\\|ệ" "e"]
          ["í\\|ì\\|î\\|ï\\|ī\\|ǐ\\|ỉ\\|ị" "i"]
          ["ñ\\|ň\\|ń" "n"]
          ["ó\\|ò\\|ô\\|ö\\|õ\\|ǒ\\|ø\\|ō\\|ồ\\|ơ\\|ọ\\|ỏ\\|ố\\|ổ\\|ỗ\\|ộ\\|ớ\\|ờ\\|ở\\|ợ" "o"]
          ["ú\\|ù\\|û\\|ü\\|ū\\|ũ\\|ư\\|ụ\\|ủ\\|ứ\\|ừ\\|ử\\|ữ\\|ự"     "u"]
          ["ý\\|ÿ\\|ỳ\\|ỷ\\|ỹ"     "y"]
          ["þ" "th"]
          ["ď\\|ð\\|đ" "d"]
          ["ĩ" "i"]
          ["ľ\\|ĺ\\|ł" "l"]
          ["ř\\|ŕ" "r"]
          ["š\\|ś" "s"]
          ["ť" "t"]
          ["ž\\|ź\\|ż" "z"]
          [" " " "]       ; thin space etc
          ["–" "-"]       ; dash
          ["—\\|一" "--"] ; em dash etc
          ])
        $begin $end
        )
    (if (null @begin)
        (if (use-region-p)
            (setq $begin (region-beginning) $end (region-end))
          (setq $begin (line-beginning-position) $end (line-end-position)))
      (setq $begin @begin $end @end))
    (let ((case-fold-search t))
      (save-restriction
        (narrow-to-region $begin $end)
        (mapc
         (lambda ($pair)
           (goto-char (point-min))
           (while (search-forward-regexp (elt $pair 0) (point-max) t)
             (replace-match (elt $pair 1))))
         $charMap)))))

(defun remove--accents (@string)
  "Returns a new string. European language chars are changed ot ASCII ones e.g. “café” ⇒ “cafe”.
See `xah-asciify-text'
Version 2015-06-08"
  (with-temp-buffer
      (insert @string)
      (xah-asciify-text (point-min) (point-max))
      (buffer-string)))
