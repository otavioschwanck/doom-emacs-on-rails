;;; term.el -*- lexical-binding: t; -*-

(map! :after vterm :map vterm-mode-map :ni "C-l" #'vterm-clear)

(defun better-vterm-paste ()
  (interactive)
  (+vterm-send-string (substring-no-properties (current-kill 0)) nil))

(defun better-vterm-clean ()
  (interactive)
  (vterm-send-C-c)
  (evil-insert 1))

(map! :after vterm :map vterm-mode-map :n "P" #'better-vterm-paste-before)
(map! :after vterm :map vterm-mode-map :n "p" #'better-vterm-paste)
(map! :after vterm :map vterm-mode-map :ni "M-p" #'vterm-send-up)
(map! :after vterm :map vterm-mode-map :ni "M-n" #'vterm-send-down)
(map! :after vterm :map vterm-mode-map :i "C-v" #'better-vterm-paste)
(map! :after vterm :map vterm-mode-map :i "M-v" #'better-vterm-paste)
(map! :after vterm :mode vterm-mode :n "C-c" #'better-vterm-clean)

(map! :leader :desc "Terminal" "v" #'+vterm/toggle)

(defun +vterm-toggle--create-terms ()
  (+vterm/here nil)
  (+workspaces-add-current-buffer-h)
  (evil-insert 1)
  (evil-window-vsplit)
  (+vterm/here nil)
  (+workspaces-add-current-buffer-h)
  (evil-insert 1)
  (message "Terminals created.  Go back to your code with SPC TAB [ or M-1 to M-9. Switch between terminals with M-h and M-l"))

(defvar +vterm-layouts '() "Command to be executed on terminal 1")
(defvar +vterm-commands '() "Command to execute with SPC o t")
(defvar +vterm-last-command nil "Last command executed by vterm")

(defun +add-layout-to-term-list (command)
  "Add a layout to vterm"
  (push command +vterm-layouts))

(defun +add-command-to-term-list (command &optional key)
  "Execute the command with +vterm. COMMAND = command to execute. key = Key to use with SPC j."
  (when key
    (let ((mapping (concat "j" key))
          (command-to-run (cdr command))
          (description (car command)))
      (fset (intern (concat "call-term-" key)) (eval `(lambda () (interactive) (+vterm--create-term-with-command (concat (eval ,command-to-run) "; read; exit") ,description))))
      (map! :leader :desc description mapping (intern (concat "call-term-" key)))))
  (push command +vterm-commands))

(defun +vterm-execute-command-term ()
  (interactive)
  (let ((item (completing-read "Select command: " +vterm-commands)))
    (when (not (string= item ""))
      (let* ((item-to-run (assoc item +vterm-commands))
             (command (concat (eval (cdr item-to-run)) "; read; exit")))
        (+vterm--create-term-with-command command item)))))

(defun +vterm-execute-last-comamnd ()
  "Execute last terminal command."
  (interactive)
  (when +vterm-last-command
    (+vterm--create-term-with-command (car +vterm-last-command) (cdr +vterm-last-command))))

(defun +vterm--create-term-with-command (command buffer)
  "Create a vterm with specified command"
  (interactive)
  (setq +vterm-last-command `(,command . ,buffer))
  (if (member buffer (mapcar (lambda (x) (format "%s" x)) (buffer-list)))
      (switch-to-buffer buffer)
    (progn
      (+vterm/here nil)
      (+workspaces-add-current-buffer-h)
      (rename-buffer buffer t)
      (+vterm-send-string command t))))

(defun +vterm-switch-to-terminal ()
  "Go to vterm terminals."
  (interactive)
  (let* ((terminals
          (remove nil (mapcar
                       (lambda (buf)
                         (with-current-buffer buf (and (not (string-match-p ".*vterm-popup.*" (format "%s" buf))) (when (eq major-mode 'vterm-mode) buf))))
                       (buffer-list (current-buffer)))))
         (terminal-to-go (completing-read "Select the terminal: " (mapcar (lambda (x) (format "%s" x)) terminals))))
    (when (not (string= terminal-to-go ""))
      (switch-to-buffer terminal-to-go))))

(defun +vterm-send-selected-text-to-terminal ()
  (interactive)
  (call-interactively 'evil-yank)
  (let* ((terminals
          (remove nil (mapcar
                       (lambda (buf)
                         (with-current-buffer buf (when (eq major-mode 'vterm-mode) buf)))
                       (buffer-list (current-buffer)))))
         (terminal-to-go (completing-read "Select the terminal to send region: " (mapcar (lambda (x) (format "%s" x)) terminals))))

    (when (not (string= terminal-to-go ""))
      (switch-to-buffer terminal-to-go)
      (better-vterm-paste)
      (evil-insert 1))))

(map! :desc "Switch to Terminal" :n "SPC l" #'+vterm-switch-to-terminal)
(map! :desc "Execute last terminal command" :n "SPC jj" #'+vterm-execute-last-comamnd)
(map! :desc "Send Text to Terminal" :v "SPC l" #'+vterm-send-selected-text-to-terminal)

(map! :leader :desc "Execute Terminal Command" "o t" #'+vterm-execute-command-term)

(defun +vterm-with-command-splitted (command-name commands)
  (interactive)
  (if (projectile-project-name)
      (+workspace-new (concat (projectile-project-name)" - " command-name " - C Terms"))
    (+workspace-new "Custom Terminals"))
  (+workspace/switch-to-final)
  (mapc (lambda (command)
          (+vterm/here nil)
          (+workspaces-add-current-buffer-h)
          (rename-buffer (concat command-name " - term") t)
          (when command
            (+vterm-send-string command t))
          (evil-insert 1)
          (unless (-contains? (last commands) command)
            (evil-window-vsplit))
          ) commands))

(defun +vterm-create-layout ()
  (interactive)
  (let* ((item (completing-read "Select a layout: " +vterm-layouts)))
    (when (not (string= item ""))
      (+vterm-with-command-splitted item (car (cdr (cdr (assoc item +vterm-layouts))))))))

(defun +vterm-send-string (string send-return)
  (mapc (lambda (c)
          (if (string= c "\n") (vterm-send-return)
            (pcase c
              (" " (vterm-send-space))
              (_ (vterm-send c)))))
        (s-split "" string t))
  (when send-return (vterm-send-return)))

(map! :leader :desc "Open Terminal Layout" "T" '+vterm-create-layout)

(after! vterm
  (set-popup-rule! "^\\*\\(vterm\\)?" :ttl nil :size 0.4))

(setq vterm-always-compile-module t)
