(after! evil
  (defadvice
      evil-ex-search
      (after evil-search-forward-recenter activate)
    (recenter))
  (ad-activate 'evil-ex-search))

(after! import-js
  (map! :map js2-mode-map :localleader "i" #'import-js-import)
  (map! :map js2-mode-map :localleader "f" #'import-js-fix)

  (map! :mode typescript-mode :localleader "i" #'import-js-import)
  (map! :mode typescript-mode :localleader "f" #'import-js-fix)

  (map! :mode typescript-tsx-mode :localleader "i" #'import-js-import)
  (map! :mode typescript-tsx-mode :localleader "f" #'import-js-fix)

  (defun run-import-js ()
    "Start the import-js daemon"
    (interactive)
    (kill-import-js)
    (let ((process-connection-type nil))
      (setq import-js-process (start-process "import-js" nil "importjs" "start" (format "--parent-pid=%s" (emacs-pid))))
      (set-process-filter import-js-process 'import-js-handle-data))))

(defun run-js-auto-import-daemon ()
  (unless (get-process "import-js")
    (run-import-js)))

(add-hook 'js2-mode-hook 'run-js-auto-import-daemon)
(add-hook 'typescript-mode-hook 'run-js-auto-import-daemon)
(add-hook 'typescript-tsx-mode-hook 'run-js-auto-import-daemon)
