;;; editor.el --- Base Editor Config                 -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Doe

;; Author: Otávio Schwanck dos Santos <otavioshwanck@gmail.com>
;; Keywords: Editor

;; Save all
(defun save-all-buffers ()
  (interactive)
  (save-some-buffers 0))

(map! :n "ç" #'save-all-buffers)

;; Search with tab is easier on code
(map! :nv "<tab>" #'evil-ex-search-forward)
(map! :nv "<C-tab>" #'evil-ex-search-backward)

;; Previous and next buffer
(map! :n "C-," #'previous-buffer)
(map! :n "C-;" #'next-buffer)

;; Drag stuff
(map! :v "K" #'drag-stuff-up)
(map! :v "J" #'drag-stuff-down)

;; Another shortcut for begin and end of line
(map! :nv "0" #'doom/backward-to-bol-or-indent)
(map! :nv "-" #'end-of-line)

;; Expand region is nice
(map! :n "C-<SPC>" #'er/expand-region)

;; Better way to kill current buffer
(map! :leader "k" #'kill-current-buffer)

;; Navigate between git hunks
(map! :nv "]g" #'git-gutter:next-hunk)
(map! :nv "[g" #'git-gutter:previous-hunk)

;; Search with avy
(map! :nv "C-s" #'evil-avy-goto-char-2)

;; Paste on insert mode
(map! :ieg "C-q" #'evil-paste-after)

;; Stop beign a noob!
(defun noob ()
  (interactive)
  (message "Stop using arrow keys.  Use h j k l instead."))

(map! :map (ruby-mode-map rspec-mode-map yaml-mode-map)
      [left]  #'noob
      [right] #'noob
      [up]    #'noob
      [down]  #'noob)

(map! :map (ruby-mode-map rspec-mode-map yaml-mode-map)
      :n [left]  #'noob
      :n [right] #'noob
      :n [up]    #'noob
      :n [down]  #'noob)

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
