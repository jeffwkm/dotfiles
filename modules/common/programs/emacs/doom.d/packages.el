;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; override the pinned versions of some packages
(package! consult :pin "c0d8a12bce2568298ff9bcfec1c6cb5e68ca0b61")
(package! lsp-mode :pin "62e1f68c1f2363f7ebe8f1c2762e472f3b5de46a") ; 9.0.0 pre-release
;; (package! org-roam :pin ...)

;; non-elpa packages
(package! shell-maker :recipe
  (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))
(package! chatgpt-shell :recipe
  (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el")))
(package! copilot :recipe
  (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))
(package! elsa :recipe (:host github :repo "emacs-elsa/Elsa"))
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

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
(package! gh-md)
(package! git-link)
(package! git-messenger)
(package! goto-chg)
(package! ligature)
(package! mentor)                       ; rtorrent client
(package! nameless)
(package! org-present)
(package! org-ql)
(package! org-ref)
(package! org-roam-ql)
(package! org-roam-ql-ql)
(package! org-roam-timestamps)
(package! org-roam-ui)
(package! org-super-agenda)
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
