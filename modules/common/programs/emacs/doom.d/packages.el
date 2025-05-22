;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; override the pinned versions of some packages
;; (package! consult :pin "4889458dccf842ab6223099f8a73ff8b147e9459")
;; (package! lsp-mode :pin "3afc56249e370afc334cf33a58a1f48d453d6267")
;; (package! org-roam :pin ...)

;; non-elpa packages
(package! shell-maker
  :recipe (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))
(package! chatgpt-shell
  :recipe (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el")))
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))
(package! elsa
  :recipe (:host github :repo "emacs-elsa/Elsa"))
(package! lsp-tailwindcss
  :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
(package! elysium
  :recipe (:host github :repo "lanceberge/elysium"))

(package! alert)
(package! catppuccin-theme)
(package! centered-window)
(package! disable-mouse)
(package! emacsql)
(package! eshell-prompt-extras)
(package! eterm-256color)
(package! evil-lisp-state)
(package! evil-matchit)
(package! evil-smartparens)
(package! flycheck-clojure)
(package! flycheck-pos-tip)
(package! forge)
(package! gh-md)
(package! git-link)
(package! git-messenger)
(package! goto-chg)
(package! gptel)
(package! ligature)
(package! mentor)                       ; rtorrent client
(package! nameless)
(package! org-fancy-priorities)
(package! org-present)
(package! org-ql)
(package! org-ref)
(package! org-roam-ql)
(package! org-roam-ql-ql)
(package! org-roam-timestamps)
(package! org-roam-ui)
(package! org-super-agenda)
(package! org-superstar)
(package! paradox)                      ; package manager
(package! paren-face)
(package! rainbow-mode)
(package! shut-up)
(package! volatile-highlights)
(package! whitespace)

;; major modes
(package! groovy-mode)
(package! nginx-mode)
(package! pkgbuild-mode)
(package! systemd)
(package! vimrc-mode)
(package! yaml-mode)
