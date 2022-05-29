;;; git.el -*- lexical-binding: t; -*-

(after! smerge-mode
  (map! :mode smerge-mode-map :leader :desc "Git Select Other" "gdo" #'smerge-keep-other)
  (map! :mode smerge-mode-map :leader :desc "Git Keep Mine" "gdm"  #'smerge-keep-mine)
  (map! :mode smerge-mode-map :leader :desc "Git Keep All" "gda" #'smerge-keep-all)
  (map! :mode smerge-mode-map :leader :desc "Git Keep at cursor" "gdc" #'smerge-keep-current))

;; Improve Magit Performance
(after! magit
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (setq magit-diff-highlight-indentation nil)
  (setq magit-diff-highlight-trailing nil)
  (setq magit-diff-paint-whitespace nil)
  (setq magit-diff-highlight-hunk-body nil)
  (setq magit-diff-refine-hunk nil)

  (defun endless/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (magit-get-push-remote)
                         "url"))
             (magit-get-current-branch)))))
