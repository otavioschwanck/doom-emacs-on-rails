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

  (defconst endless/gitlab-pr-format
    (concat "https://gitlab.com/%s/merge_requests/new?"
            "merge_request[target_branch]=%s&"
            "merge_request[source_branch]=%s"
            "%s")
    "Merge request URL format for GitLab.")

  (defconst endless/github-pr-format
    "https://github.com/%s/compare/%s...%s?expand=1&assignee=Malabarba%s"
    "Pull request URL format for GitHub.")

  (defvar endless/pr-body-format nil)

  (defvar endless/pr-additional-params "")

  (defun endless/issue-md-link-from-id (id)
    (format "[%s](https://atlassian.net/browse/%s)"
            id id))

  (defun endless/branch-to-title-and-link (branch)
    "Return a title string derived from current branch."
    (when (string-match "\\`\\([[:alpha:]]+\\(?:-[0-9]+\\)\\)-\\(.*\\)\\'" branch)
      (let ((prefix (upcase (match-string 1 branch)))
            (title (match-string 2 branch)))
        (let* ((case-fold-search nil)
               (spaced (replace-regexp-in-string "-+" " " title))
               (capitalized (if (string-match "[A-Z]" spaced) spaced (capitalize spaced))))
          `((title . ,(format "[%s] %s" prefix capitalized))
            (link . ,(endless/issue-md-link-from-id prefix)))))))

  (defun endless/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (let* ((base-branch (replace-regexp-in-string
                         "^origin/" ""
                         (or (magit-get-upstream-branch) "master")))
           (upstream "origin")
           (remote-url (magit-get "remote" upstream "url"))
           (current-branch (magit-get-current-branch))
           ;; (branch-desc (magit-get "branch" current-branch "description"))
           )
      (browse-url
       (format
        (if (string-match "gitlab\\.com:" remote-url)
            endless/gitlab-pr-format
          endless/github-pr-format)
        (replace-regexp-in-string
         "\\`.+git...\\.com:\\(.+\\)\\.git\\'" "\\1"
         remote-url)
        base-branch
        current-branch
        (let-alist (endless/branch-to-title-and-link current-branch)
          (let ((body-param (when endless/pr-body-format
                              (thread-last .link
                                (format endless/pr-body-format)
                                (url-encode-url)
                                (replace-regexp-in-string "#" "%23")
                                (concat "&body="))))
                (title-param (when .title
                               (concat "&title=" (url-encode-url .title)))))
            (when (and .link (not body-param))
              (kill-new .link))
            (concat endless/pr-additional-params title-param body-param)))))))
  (map! :map magit-status-mode-map "C-q" #'endless/visit-pull-request-url))
