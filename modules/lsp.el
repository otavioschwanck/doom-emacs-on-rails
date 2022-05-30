;;; lsp.el -*- lexical-binding: t; -*-

;; improve LSP
(after! lsp-mode
  (setq lsp-auto-guess-root t)
  (setq lsp-solargraph-symbols nil)
  (setq lsp-solargraph-folding nil))

(after! lsp-mode
  (setq lsp-disabled-clients '(emmet-ls))
  (setq lsp-ui-sideline-show-code-actions t))
