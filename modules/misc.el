;;; misc.el -*- lexical-binding: t; -*-

;; Google Translate

(after! google-translate
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130)))

(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "pt")

(map! :desc "Google Translate" :nv "C-c ! t" 'google-translate-query-translate)
(map! :desc "Google Translate Reverse" :nv "C-c ! T" 'google-translate-query-translate-reverse)
(map! :desc "Google Translate At Point" :nv "C-c t" 'google-translate-at-point)
(map! :desc "Google Translate At Point reverse" :nv "C-c T" 'google-translate-at-point-reverse)

(after! google-translate-default-ui
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130)))


;; Config Stuff
(map! :leader :desc "Upgrade Doom Emacs on Rails" "h U" #'upgrade-doom-emacs-on-rails)
(map! :leader :desc "Visit handbook" "f h" #'visit-handbook)

(defun visit-handbook ()
  "Visit the user-settings.el."
  (interactive)
  (find-file (concat doom-private-dir "docs/emacs-handbook.org"))
  (message "Welcome to Doom Emacs Handbook!")
  (read-only-mode))


(add-to-list '+doom-dashboard-menu-sections '("Open Doom Emacs on Rails Handbook"
                                              :icon (all-the-icons-octicon "ruby" :face 'doom-dashboard-menu-title)
                                              :action visit-handbook) t)

(add-to-list '+doom-dashboard-menu-sections '("Upgrade Doom Emacs On Rails"
                                              :icon (all-the-icons-octicon "cloud-upload" :face 'doom-dashboard-menu-title)
                                              :action upgrade-doom-emacs-on-rails) t)

;; Just close emacs please
(setq confirm-kill-emacs nil)

;; Change logo?
(let ((logo (concat doom-private-dir "logo.png")))
  (when (file-exists-p logo)
  (setq fancy-splash-image logo)))

;; Remove Accents
(defun remove-accents (&optional @begin @end)
  "Remove accents in some letters and some
Change European language characters into equivalent ASCII ones, e.g. “café” ⇒ “cafe”.
When called interactively, work on current line or text selection.

URL `http://ergoemacs.org/emacs/emacs_zap_gremlins.html'
Version 2018-11-12"
  (interactive)
  (let (($charMap
         [
          ["ß" "ss"]
          ["á\\|à\\|â\\|ä\\|ā\\|ǎ\\|ã\\|å\\|ą\\|ă\\|ạ\\|ả\\|ả\\|ấ\\|ầ\\|ẩ\\|ẫ\\|ậ\\|ắ\\|ằ\\|ẳ\\|ặ" "a"]
          ["æ" "ae"]
          ["ç\\|č\\|ć" "c"]
          ["é\\|è\\|ê\\|ë\\|ē\\|ě\\|ę\\|ẹ\\|ẻ\\|ẽ\\|ế\\|ề\\|ể\\|ễ\\|ệ" "e"]
          ["í\\|ì\\|î\\|ï\\|ī\\|ǐ\\|ỉ\\|ị" "i"]
          ["ñ\\|ň\\|ń" "n"]
          ["ó\\|ò\\|ô\\|ö\\|õ\\|ǒ\\|ø\\|ō\\|ồ\\|ơ\\|ọ\\|ỏ\\|ố\\|ổ\\|ỗ\\|ộ\\|ớ\\|ờ\\|ở\\|ợ" "o"]
          ["ú\\|ù\\|û\\|ü\\|ū\\|ũ\\|ư\\|ụ\\|ủ\\|ứ\\|ừ\\|ử\\|ữ\\|ự"     "u"]
          ["ý\\|ÿ\\|ỳ\\|ỷ\\|ỹ"     "y"]
          ["þ" "th"]
          ["ď\\|ð\\|đ" "d"]
          ["ĩ" "i"]
          ["ľ\\|ĺ\\|ł" "l"]
          ["ř\\|ŕ" "r"]
          ["š\\|ś" "s"]
          ["ť" "t"]
          ["ž\\|ź\\|ż" "z"]
          [" " " "]       ; thin space etc
          ["–" "-"]       ; dash
          ["—\\|一" "--"] ; em dash etc
          ])
        $begin $end
        )
    (if (null @begin)
        (if (use-region-p)
            (setq $begin (region-beginning) $end (region-end))
          (setq $begin (line-beginning-position) $end (line-end-position)))
      (setq $begin @begin $end @end))
    (let ((case-fold-search t))
      (save-restriction
        (narrow-to-region $begin $end)
        (mapc
         (lambda ($pair)
           (goto-char (point-min))
           (while (search-forward-regexp (elt $pair 0) (point-max) t)
             (replace-match (elt $pair 1))))
         $charMap)))))

(defun remove--accents (@string)
  "Returns a new string. European language chars are changed ot ASCII ones e.g. “café” ⇒ “cafe”.
See `xah-asciify-text'
Version 2015-06-08"
  (with-temp-buffer
    (insert @string)
    (xah-asciify-text (point-min) (point-max))
    (buffer-string)))

(defun upgrade-doom-emacs-on-rails ()
  "Pull, Sync and upgrade el file."
  (interactive)
  (message "Upgrading... Please wait...")
  (package-refresh-contents)
  (+vterm--create-term-with-command (concat "cd " doom-private-dir "; git pull -f; " doom-emacs-dir "bin/doom sync -u; echo 'update complete.  Press enter to close.'; read; exit") "Doom On Emacs On Rails - Upgrade")
  (message "Upgrade done!  Please restart your emacs."))

(defun visit-config-utils ()
  "Visit the config.el."
  (interactive)
  (find-file (concat doom-private-dir "user/config.el"))
  (message "Welcome to your settings file!"))

(defun visit-user-packages ()
  "Visit the packages.el."
  (interactive)
  (find-file (concat doom-private-dir "user/packages.el"))
  (message "Welcome to your packages file!"))

(defun visit-user-init ()
  "Visit the init.el."
  (interactive)
  (find-file (concat doom-private-dir "user/init.el"))
  (message "Welcome to your init file!"))

(map! :leader :desc "Visit User Config" "fm" 'visit-config-utils)
(map! :leader :desc "Visit User Init" "fi" 'visit-user-init)
(map! :leader :desc "Visit User Packages" "fI" 'visit-user-packages)

(when (not (file-exists-p "~/.pryrc")) (shell-command "cp ~/.doom.d/user/examples/.pry-example ~/.pryrc"))
(when (not (file-exists-p "~/.irbrc")) (shell-command "cp ~/.doom.d/user/examples/.irbrc-example ~/.irbrc"))

(defun reload-user-settings ()
  "Pull, Sync and upgrade el file"
  (interactive)
  (load (expand-file-name "user/config.el" doom-private-dir))
  (doom/reload-font)
  (doom/reload-theme))
