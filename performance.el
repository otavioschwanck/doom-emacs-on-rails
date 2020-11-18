;;; performance.el -*- lexical-binding: t; -*-

(setq read-process-output-max (* 1024 1024)) ; 1mb.
(remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally)
(with-eval-after-load "ivy-rich"
  (setq ivy-rich-parse-remote-buffer nil))

(setq auto-window-vscroll nil)

(setq fast-but-imprecise-scrolling 't)
(setq jit-lock-defer-time 0)
