;;; editor.el -*- lexical-binding: t; -*-

;; Better Save

(defun save-all-buffers ()
  (interactive)
  (save-some-buffers 0))

(map! :n "ç" #'save-all-buffers)
(map! :n "\\" #'save-all-buffers)

;; Improve Doom One
(when (eq doom-theme 'doom-one)
  (custom-set-faces
   '(line-number ((t (:inherit default :foreground "gray40" :strike-through nil :underline nil :slant normal :weight normal))))))

;; Open Editor in Full Screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Harpoon
(map! :n "," 'harpoon-quick-menu-hydra)

;; Fix Smartparens on Ruby
(add-hook! 'ruby-mode-hook (sp-local-pair 'ruby-mode "{" "}" :actions '(wrap insert autoskip navigate) :unless '(sp-point-before-word-p sp-point-before-same-p) :post-handlers '(("||
[i]" "RET") ("| " "SPC"))))

;; Improve Margin
(setq scroll-margin 3)

;; Toggle Case
(map! "M-c" 'string-inflection-toggle)
(map! "M-S-c" 'string-inflection-cycle)

;; Default Identation
(after! web-mode
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(after! js2-mode
  (setq js-indent-level 2)
  (setq indent-tabs-mode nil))

(after! rjsx-mode
  (setq js-indent-level 2)
  (setq indent-tabs-mode nil))

;; Indent Whole File
(defun indent-whole-buffer ()
  "INDENT WHOLE BUFFER."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(map! :desc "Indent File" :leader "-" #'indent-whole-buffer)

;; Buffer previous and next
(map! :ni "C-h" #'previous-buffer)
(map! :ni "C-l" #'next-buffer)
;;
;; Buffer previous and next
(map! :ni "C-," #'previous-buffer)
(map! :ni "C-;" #'next-buffer)

;; Drag Stuff
(map! :v "K" #'drag-stuff-up)
(map! :v "J" #'drag-stuff-down)

;; Better beg and end of line
(map! :nv "0" #'doom/backward-to-bol-or-indent)
(map! :nv "-" #'end-of-line)

;; Kill Buffer with SPC k
(map! :desc "Kill Buffer" :leader "k" #'kill-current-buffer)

;; Easy access for cool searchs
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(map! :nv "s" #'evil-avy-goto-word-1)
(map! :n "S" #'+default/search-buffer)

(map! :n ";" #'consult-imenu)

;; Better paste after
(defun better-paste-after ()
  (interactive)
  (yank))

(map! :ig "C-v" #'better-paste-after)
(map! :ig "M-v" #'better-paste-after)

;; Improve Multiple Cursors
(map! :n "C-M-d" #'evil-multiedit-match-all)

(after! evil-multiedit
  (map! :map iedit-occurrence-keymap-default
        "M-D" nil))

;; Mac improvement (cmd = meta)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'control)

;; Fix yaml lint
(setq flycheck-yamllintrc ".yamllint.yml")

;; Open In line
(defadvice find-file-noselect (around find-file-noselect-at-line
                                      (filename &optional nowarn rawfile wildcards)
                                      activate)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename))
           (buffer-name ad-do-it))
      (when line-number
        (with-current-buffer buffer-name
          (goto-char (point-min))
          (forward-line (1- line-number)))))))

;; Improve avy
(setq avy-single-candidate-jump t)
(setq evil-want-visual-char-semi-exclusive t)

;; Window Control
(map! :ni "M-k" #'evil-window-up)
(map! :ni "M-j" #'evil-window-down)
(map! :ni "M-h" #'evil-window-left)
(map! :ni "M-l" #'evil-window-right)

;; VTerm Window
(map! :map vterm-mode-map "M-k" #'evil-window-up)
(map! :map vterm-mode-map "M-j" #'evil-window-down)
(map! :map vterm-mode-map "M-h" #'evil-window-left)
(map! :map vterm-mode-map "M-l" #'evil-window-right)

(after! tide
  (map! :map tide-mode-map :localleader "f" #'tide-fix)
  (map! :map tide-mode-map :localleader "R" #'tide-rename-symbol)
  (map! :map tide-mode-map :localleader "F" #'tide-rename-file))

(after! evil-org
  (map! :map evil-org-mode-map :niv "M-k" #'evil-window-up)
  (map! :map evil-org-mode-map :niv "M-j" #'evil-window-down)
  (map! :map evil-org-mode-map :niv "M-h" #'evil-window-left)
  (map! :map evil-org-mode-map :niv "M-l" #'evil-window-right)
  (map! :map evil-org-mode-map :niv "C-M-k" #'org-metaup)
  (map! :map evil-org-mode-map :niv "C-M-j" #'org-metadown))

(map! "M-o" #'evil-window-next)
(map! ";" #'evil-window-next)

(setq evil-split-window-below t evil-vsplit-window-right t)

;; Emmet
(map! :after web-mode :map web-mode-map :i "M-e" #'emmet-expand-yas)
(map! :after js2-mode :map rjsx-mode-map :i "M-e" #'emmet-expand-yas)

(defun otavio/swap-arg-forward ()
  (interactive)
  (evil-exchange (nth 0 (evil-inner-arg)) (nth 1 (evil-inner-arg)))
  (evil-forward-arg 1)
  (evil-exchange (nth 0 (evil-inner-arg)) (nth 1 (evil-inner-arg))))

(defun otavio/swap-arg-backward ()
  (interactive)
  (evil-exchange (nth 0 (evil-inner-arg)) (nth 1 (evil-inner-arg)))
  (evil-backward-arg 1)
  (evil-exchange (nth 0 (evil-inner-arg)) (nth 1 (evil-inner-arg))))

(map! :n "gl" #'otavio/swap-arg-forward)
(map! :n "gh" #'otavio/swap-arg-backward)
(map! :nv "C-j" "C-M-n")
(map! :nv "C-k" "C-M-p")

;; Multiline
(after! evil
  (define-key evil-normal-state-map (kbd "g S") #'multi-line)
  (define-key evil-normal-state-map (kbd "g J") #'multi-line-single-line))

(defvar debugger-command "require 'pry'; binding.pry")
(defvar pry-show-helper nil)

(defun otavio/remove-all-debuggers ()
  (interactive)
  (setq CURRENT_LINE (line-number-at-pos))
  (setq DELETATIONS 0)
  (goto-char (point-min))
  (while (search-forward debugger-command (point-max) t)
    (beginning-of-line)
    (kill-line 1)
    (setq DELETATIONS (1+ DELETATIONS)))
  (goto-char (point-min))
  (forward-line (- (1- CURRENT_LINE) DELETATIONS))
  (save-buffer))

(defun otavio/insert-debugger ()
  (interactive)
  (setq HELPER (if pry-show-helper " # next; step; break; break 14;break FooBar#func;break --help;" ""))
  (setq REAL_COMMAND (if (eq major-mode 'ruby-mode) (concat debugger-command HELPER) (concat "<% " debugger-command HELPER " %>")))
  (back-to-indentation)
  (newline-and-indent)
  (forward-line -1)
  (insert REAL_COMMAND)
  (indent-according-to-mode)
  (save-buffer))

(map! :after ruby-mode :map ruby-mode-map :desc "Insert debugger" :leader "d" 'otavio/insert-debugger)
(map! :after ruby-mode :map ruby-mode-map :desc "Remove All Debuggers" :leader "D" 'otavio/remove-all-debuggers)
(map! :after web-mode :mode web-mode-map :desc "Insert Debugger" :leader "d" 'otavio/insert-debugger)
(map! :after web-mode :mode web-mode-map :desc "Remove All Debuggers" :leader "D" 'otavio/remove-all-debuggers)
