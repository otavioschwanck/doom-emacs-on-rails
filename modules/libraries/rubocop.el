;;; rubocop.el -*- lexical-binding: t; -*-

(after! ruby-mode
  (defun add-rubocop-at-point (errors)
    "add rubocop at point"
    (let* ((error_message (flycheck-popup-tip-format-errors errors))
           (cop_to_toggle
            (save-match-data
              (and (string-match "\\([a-zA-Z]+/[a-zA-Z]*\\)" error_message)
                   (match-string 1 error_message)))))
      (beginning-of-line)
      (if (search-forward " # rubocop:disable" (point-at-eol) t)
          (progn
            (end-of-line)
            (insert (concat ", " cop_to_toggle))
            (beginning-of-line-text)
            (save-buffer)
            )
        (progn )
        (end-of-line)
        (insert (concat " # rubocop:disable " cop_to_toggle))
        (beginning-of-line-text)
        (save-buffer))))

  (defun rubocop-remove-if-exists ()
    (interactive)
    (beginning-of-line)
    (when (search-forward " # rubocop:disable" (point-at-eol) t)
      (progn
        (search-backward "#")
        (kill-visual-line))))

  (defun rubocop-toggle-at-point ()
    "Toggle the rubocop at point."
    (interactive)
    (-if-let (errors (flycheck-overlay-errors-at (point)))
        (add-rubocop-at-point errors) (rubocop-remove-if-exists))))
