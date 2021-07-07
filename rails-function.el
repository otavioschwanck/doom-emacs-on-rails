(defun ruby-move-selection-to-new-method ()
  (interactive)
  (call-interactively 'evil-delete)
  (evil-force-normal-state)
  (let* ((initial_point (point))
         (private_exists
          (progn
            (goto-char (point-min))
            (search-forward "private" (point-max) t)
            )
          ))

    (let* ((method_name (read-string "Por favor, digite o nome do método que você quer utilizar (Para por os parâmetros, já adiciona agora): ")))
      (if private_exists (ruby-insert-asking-for-private initial_point method_name) (ruby-insert-after-method initial_point method_name))
      )
    )
  )

(defun ruby-insert-asking-for-private (initial_point method_name)
  (if (yes-or-no-p "private encontrado no arquivo, deseja adicionar após o private?")
      (progn
        (goto-char initial_point)
        (insert-line-above)
        (evil-previous-line)
        (insert method_name)
        (evil-indent (point-at-bol) (point-at-eol))
        (goto-char (point-min))
        (search-forward "private" (point-max) t)
        (insert-line-below)
        (evil-next-line 1)
        (insert-ruby-method method_name)
        )
    (progn
      (goto-char initial_point)
      (ruby-insert-after-method initial_point method_name)
      )
    ))

(defun ruby-insert-after-method (initial_point method_name)
  (goto-char initial_point)
  (insert-line-above)
  (evil-previous-line)
  (insert method_name)
  (evil-indent (point-at-bol) (point-at-eol))
  (+evil/next-end-of-method)
  (insert-line-above)
  (evil-previous-line)
  (insert-ruby-method method_name)
  )

(defun insert-ruby-method (method_name)
  (insert (concat "def " method_name))
  (evil-indent (point-at-bol) (point-at-eol))
  (setq inicio_do_metodo (point))
  (insert-line-below)
  (evil-next-line)
  (yank)
  (insert "end")
  (setq final_do_metodo (point))
  (evil-indent (point-at-bol) (point-at-eol))
  (evil-indent inicio_do_metodo final_do_metodo)
  (goto-char inicio_do_metodo)
  (insert-line-above)
  (goto-char initial_point)
  )

(defun insert-line-below ()
  "Insert an empty line below the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

(defun insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (end-of-line 0)
    (open-line 1)))

(defun line-length (n)
  "Length of the Nth line."
  (save-excursion
    (goto-char (point-min))
    (if (zerop (forward-line (1- n)))
        (- (line-end-position)
           (line-beginning-position)))))
