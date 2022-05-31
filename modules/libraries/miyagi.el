;;; miyagi.el --- Mr Miyagi Stuff                    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Otávio Schwanck dos Santos

;; Author: Otávio Schwanck dos Santos <otavioschwanck@gmail.com>
;; Keywords: tools, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defvar miyagi-buffer-name "Mr Miyagi" "Buffer name for mr miyagi")
(defvar miyagi-exercise-count 8 "Buffer name for mr miyagi")

(defun mr-miyagi ()
  "Lets train some vim and emacs commands?"
  (interactive)
  (get-buffer-create miyagi-buffer-name)
  (set-buffer miyagi-buffer-name)
  (switch-to-buffer miyagi-buffer-name)
  (delete-region (point-min) (point-max))
  (insert
   (with-temp-buffer
     (insert-file-contents (expand-file-name "docs/miyagi" doom-private-dir))
     (buffer-string)))
  (ruby-mode)
  (goto-char (point-min))
  (mr-miyagi-mode))

(defun miyagi-next ()
  (interactive)
  (when (< miyagi-current-exercise miyagi-exercise-count)
    (goto-char (point-min))
    (setq-local miyagi-current-exercise (+ 1 miyagi-current-exercise))
    (search-forward (concat "EXERCISE " (format "%s" miyagi-current-exercise) " HERE:"))
    (recenter)
    (miyagi-generate-popup)))

(defun miyagi-previous ()
  (interactive)
  (when (> miyagi-exercise-count 1)
    (goto-char (point-min))
    (setq-local miyagi-current-exercise (- miyagi-current-exercise 1))
    (search-forward (concat "EXERCISE " (format "%s" miyagi-current-exercise) " HERE:"))
    (recenter)
    (miyagi-generate-popup)))

(defun miyagi--create-help-buffer (exercise commands)
  (interactive)
  (delete-other-windows)
  (get-buffer-create "*Miyagi Help*")
  (set-buffer "*Miyagi Help*")
  (switch-to-buffer "*Miyagi Help*")
  (delete-region (point-min) (point-max))
  (insert exercise "\n" "Commands:" "\n" commands)
  (evil-window-split)
  (evil-window-up 1)
  (evil-window-decrease-height 5)
  (evil-window-down 1)
  (switch-to-buffer miyagi-buffer-name))

(defun miyagi-generate-popup ()
  (interactive)
  (save-excursion
    (search-backward "Before:")
    (let ((exercise (miyagi--get-exercise))
          (commands (miyagi--get-commands)))
      (miyagi--create-help-buffer exercise commands)))
  (forward-line 1)
  (search-forward " # < START LINE")
  (goto-char (point-at-bol))
  (when (looking-at " ") (evil-forward-word-begin))
  (recenter))

(defun miyagi--get-exercise ()
  (save-excursion
    (let ((beg (point))
          (end (save-excursion (search-forward "COMMANDS TO EXECUTE") (forward-line -1) (point))))
      (buffer-substring beg end))))

(defun miyagi--get-commands ()
  (search-forward "COMMANDS TO EXECUTE")
  (save-excursion
    (let ((beg (point))
          (end (save-excursion (search-forward "###\n") (forward-line -1) (point))))
      (buffer-substring beg end))))

;;;autoload
(define-minor-mode mr-miyagi-mode
  "Mode for my-miyagi."
  :global nil
  :lighter " routes"
  (map! :mode mr-miyagi-mode :leader "z j" 'miyagi-next)
  (map! :mode mr-miyagi-mode :leader "z k" 'miyagi-previous)
  (setq-local miyagi-current-exercise 0)
  (message "Welcome to mr miyagi."))

(provide 'miyagi)
;;; miyagi.el ends here
