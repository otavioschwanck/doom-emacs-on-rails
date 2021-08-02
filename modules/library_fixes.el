(after! emmet-mode
  (defvar emmet-expand-jsx-htmlFor? nil)

  (defun emmet-make-html-tag (tag-name tag-has-body? tag-id tag-classes tag-props tag-txt settings content)
    "Create HTML markup string"
    (emmet-aif
     (gethash tag-name emmet-tag-snippets-table)

     (let ((fn (if (stringp it)
                   (emmet-html-snippets-instantiate-lambda it)
                 it)))
       (prog1
           (funcall fn content)
         (puthash tag-name fn emmet-tag-snippets-table)))

     (let* ((id           (emmet-concat-or-empty " id=\"" tag-id "\""))
            (class-attr  (if emmet-expand-jsx-className? " className=\"" " class=\""))
            (classes      (emmet-mapconcat-or-empty class-attr tag-classes " " "\""))
            (props        (let* ((tag-props-default
                                  (and settings (gethash "defaultAttr" settings)))
                                 (merged-tag-props
                                  (emmet-merge-tag-props
                                   tag-props-default
                                   tag-props)))
                            (emmet-mapconcat-or-empty
                             " " merged-tag-props " " nil
                             (lambda (prop)
                               (let ((key (car prop)))
                                 (concat (if (symbolp key) (symbol-name key)
                                           (if (and emmet-expand-jsx-htmlFor?
                                                    (string= key "for")) "htmlFor" key))
                                         "=\"" (cadr prop) "\""))))))
            (content-multiline? (and content (string-match "\n" content)))
            (block-tag?         (and settings (gethash "block" settings)))
            (self-closing?      (and (not (or tag-txt content))
                                     (or (not tag-has-body?)
                                         (and settings (gethash "selfClosing" settings)))))
            (block-indentation? (or content-multiline? (and block-tag? content)))
            (lf                 (if block-indentation? "\n")))
       (concat "<" tag-name id classes props
               (if self-closing?
                   (concat emmet-self-closing-tag-style ">")
                 (concat ">"
                         (if tag-txt
                             (if block-indentation?
                                 (emmet-indent tag-txt)
                               tag-txt))
                         (if content
                             (if block-indentation?
                                 (emmet-indent content)
                               content))
                         lf
                         "</" tag-name ">")))))))

;; Projectile Rails
(after! projectile-rails
(defun projectile-rails--ruby-mode-indent-tabs-p ()
  (with-temp-buffer
    (set-auto-mode)
    indent-tabs-mode))
(defun projectile-rails-corresponding-snippet ()
  "Call `projectile-rails--expand-snippet' with a snippet corresponding to the current file."
  (let* ((name (buffer-file-name))
         (snippet
          (cond ((string-match "app/[^/]+/concerns/\\(.+\\)\\.rb$" name)
                 (format
                  "module %s\n  extend ActiveSupport::Concern\n  $0\nend"
                  (s-join "::" (projectile-rails-classify (match-string 1 name)))))
                ((string-match "app/controllers/\\(.+\\)\\.rb$" name)
                 (format
                  "class %s < ${1:ApplicationController}\n$2\nend"
                  (s-join "::" (projectile-rails-classify (match-string 1 name)))))
                ((string-match "app/jobs/\\(.+\\)\\.rb$" name)
                 (format
                  "class %s < ${1:ApplicationJob}\n$2\nend"
                  (s-join "::" (projectile-rails-classify (match-string 1 name)))))
                ((string-match "spec/[^/]+/\\(.+\\)_spec\\.rb$" name)
                 (format
                  "require '${1:rails_helper}'\n\nRSpec.describe %s do\n  $0\nend"
                  (s-join "::" (projectile-rails-classify (match-string 1 name)))))
                ((string-match "app/models/\\(.+\\)\\.rb$" name)
                 (projectile-rails--snippet-for-model (match-string 1 name)))
                ((string-match "app/helpers/\\(.+\\)_helper\\.rb$" name)
                 (format
                  "module %sHelper\n$1\nend"
                  (s-join "::" (projectile-rails-classify (match-string 1 name)))))
                ((string-match "lib/\\(.+\\)\\.rb$" name)
                 (projectile-rails--snippet-for-module "${1:module} %s\n" name))
                ((string-match "app/\\(?:[^/]+\\)/\\(.+\\)\\.rb$" name)
                 (projectile-rails--snippet-for-module "${1:class} %s\n" name)))))
    (if (and snippet projectile-rails-expand-snippet-with-magic-comment)
        (format "# frozen_string_literal: true\n\n%s" snippet)
      snippet)))
  )

(after! evil
  (defadvice
      evil-ex-search
      (after evil-search-forward-recenter activate)
    (recenter))
  (ad-activate 'evil-ex-search))
