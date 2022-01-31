;; Please dont modify this file.  Edit ~/.doom.d/user-init.el instead.

(doom! :completion
       company             ; the ultimate code completion backend
       (vertico +icons)           ; the search engine of the future

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink cursor line after big motions
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       (treemacs +lsp)          ; a project drawer, like neotree but cooler
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       format            ; automated prettiness
       multiple-cursors  ; editing in many places at once
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to

       :emacs
       (dired +icons)             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       grammar           ; tasing grammar mistake every you make

       :tools
       direnv
       docker
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       lsp               ; M-x vscode
       (magit +forge)             ; a git porcelain for Emacs
       pdf               ; pdf enhancements
       rgb               ; creating color strings
       taskrunner        ; taskrunner for all your projects
       tmux              ; an API for interacting with tmux

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       tty

       :lang
       (csharp +lsp +dotnet)            ; unity, .NET, and mono shenanigans
       emacs-lisp        ; drown in parentheses
       (json +lsp)              ; At least it ain't XML
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       markdown          ; writing docs for people to ignore
       org               ; organize your plain life in plain text
       (python +lsp +pyright +pyenv) ; beautiful is better than ugly
       rest              ; Emacs as a REST client
       (ruby +rails +rbenv +lsp)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       solidity          ; do you need a blockchain? No.
       (web +lsp)  ; the tubes
       (yaml +lsp)              ; JSON, but readable

       :app
       everywhere        ; *leave* Emacs!? You must be joking

       :config
       (default +bindings +smartparens))

(if (file-exists-p (expand-file-name "user-init.el" doom-private-dir))
    (load (expand-file-name "user-init.el" doom-private-dir))
  (progn
    (shell-command "cp ~/.doom.d/user-init.example.el ~/.doom.d/user-init.el")
    (load (expand-file-name "user-init.el" doom-private-dir))))
