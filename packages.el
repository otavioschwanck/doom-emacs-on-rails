;; Please dont modify this file.  Edit ~/.doom.d/user-packages.el instead.

(package! string-inflection :pin "fd7926ac17293e9124b31f706a4e8f38f6a9b855")
(package! kubernetes :pin "d52ad7dacf17b659060e52d5e3318cafd7946616")
(package! kubernetes-evil :pin "d52ad7dacf17b659060e52d5e3318cafd7946616")
(package! multi-line :pin "625c608443f98bb34b4d5600d52c198509fb64d0")
(package! ruby-refactor :pin "e6b7125878a08518bffec6942df0c606f748e9ee")
(package! command-log-mode :pin "af600e6b4129c8115f464af576505ea8e789db27")
(package! google-translate :pin "0f7f48a09bca064999ecea03102a7c96f52cbd1b")
(package! ruby-hash-syntax :pin "d458fb5891e0da85271b1cba3ee0ee69ea66a374")
(package! f :pin "50af874cd19042f17c8686813d52569b1025c76a")
(package! lsp-tailwindcss :pin "dc4d5246afe8620cdffaff1a362529f5d63b1ef5")
(package! crdt :pin "1b8af8112944cb79c66a35b4404f61ac2abfef1d")
(package! rails-routes :pin "eab995a9297ca5bd9bd4f4c2737f2fecfc36def0")
(package! rails-i18n :pin "8e87e4e48e31902b8259ded28a208c2e7efea6e9")
(package! harpoon :pin "a23571eaab94fb2da0569ed5ab3c1b469f123b97")
(package! ruby-json-to-hash :pin "383b22bb2e007289ac0dba146787d02ff99d4415")
(package! evil-tutor :pin "4e124cd3911dc0d1b6817ad2c9e59b4753638f28")

(if (file-exists-p (expand-file-name "user-packages.el" doom-private-dir))
    (load (expand-file-name "user-packages.el" doom-private-dir))
  (progn
    (shell-command "cp ~/.doom.d/user-packages.example.el ~/.doom.d/user-packages.el")
    (load (expand-file-name "user-packages.el" doom-private-dir))))
