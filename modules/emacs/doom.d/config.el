;; -*- lexical-binding: t -*-

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! dash)
(use-package! s)
(use-package! f)
(use-package! ht)

(menu-bar-mode -1)

(setq user-full-name "Jeff Workman"
      user-mail-address "jeff.workman@gmail.com"
      doom-leader-key "SPC"
      doom-leader-alt-key "C-SPC"
      smie-indent-basic 2
      +workspaces-on-switch-project-behavior 'non-empty
      display-line-numbers-type nil
      emojify-download-emojis-p t
      org-directory "~/org/"
      confirm-kill-processes nil
      frame-title-format '((:eval (concat (if (buffer-file-name)
                                              (abbreviate-file-name (buffer-file-name))
                                            "%b")
                                          ;; " | <Emacs>"
                                          )))
      require-final-newline t)

(setq-default tab-width 2
              fill-column 100
              byte-compile-warning-types '(not free-vars constants mutate-constant)
              lisp-indent-offset nil)

(defun --window-system-available () (< 0 (length (getenv "DISPLAY"))))
(defun --wayland-available () (< 0 (length (getenv "WAYLAND_DISPLAY"))))
(defun graphical? () (cl-some #'display-graphic-p (frame-list)))
(defun mac? () (eql system-type 'darwin))
(defun gui-mac-std? () (eql window-system 'ns))
(defun gui-emacs-mac? () (eql window-system 'mac))
(defun gui-mac? () (or (gui-mac-std?) (gui-emacs-mac?)))

(setq doom-theme 'doom-tomorrow-night
      ;; doom-theme 'doom-one
      doom-one-brighter-comments t
      doom-one-brighter-modeline nil
      doom-themes-padded-modeline nil)

;;; Customize some theme colors
;; (defun --customize-doom-theme-defs ()
;;   (apply
;;    custom-theme-set-faces
;;    'doom-tomorrow-night))
;; (add-hook! '(doom-after-init-hook doom-after-reload-hook) :append
;;            '--customize-doom-theme-defs)

;; (load-theme doom-theme t)
;; 'doom-one 'doom-gruvbox 'doom-tomorrow-night 'doom-spacegrey 'doom-material-dark 'doom-zenburn
(setq doom-themes-padded-modeline nil)
(setq doom-gruvbox-dark-variant "soft")
;; (setq doom-gruvbox-dark-variant "medium")
;; (setq doom-gruvbox-dark-variant "hard")

(when (mac?)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        find-function-C-source-directory
        "/Users/jeff/Library/Caches/Homebrew/emacs-plus@28--git/src/"))

(load! "commands.el")

(setq +ivy-buffer-preview t)

(defvar --large-font nil)
(defun --configure-fonts ()
  (setq doom-font (font-spec :family "JetBrainsMono Nerd Font"
                             ;; :family "FiraCode Nerd Font Medium"
                             :size (if (mac?) 14 15)
                             :weight 'semibold)
        doom-big-font nil
        doom-big-font-increment 2
        doom-font-increment 1
        doom-variable-pitch-font (font-spec :family "Noto Sans"
                                            ;; :family "Fira Sans"
                                            :size (if --large-font 17 15))))
(defun --sync-fonts ()
  (when (graphical?)
    (set-frame-font doom-font)))
(--configure-fonts)
(add-hook! 'doom-after-reload-hook  :append '--configure-fonts '--sync-fonts)

(use-package! hl-line)

;; fixes for warnings/errors on doom init and reload
(use-package! hydra)
(use-package! ivy :config (require 'ivy-hydra))

(use-package! fringe
  :config
  (add-to-list 'default-frame-alist '(right-fringe . 8))
  (add-to-list 'default-frame-alist '(left-fringe . 8))
  (fringe-mode 8))

(use-package! ruby-mode
  :mode (("\\.lic\\'" . ruby-mode)
         ("\\.rb\\'"  . ruby-mode)))

(defun --toggle-large-font ()
  (interactive)
  (setq --large-font (not --large-font))
  (--configure-fonts)
  (doom/reload-font)
  (message "--large-font %s" (if --large-font "enabled" "disabled")))

(defun --indent-tabs-on () (setq-local indent-tabs-mode t))
(defun --indent-tabs-off () (setq-local indent-tabs-mode nil))
(defun --indent-tabs-mode (hook enable-tabs)
  (add-hook hook (if enable-tabs '--indent-tabs-on '--indent-tabs-off)))

(use-package! smartparens
  :init
  (setq sp-base-key-bindings 'sp
        sp-override-key-bindings '(("C-M-<left>"  . nil)
                                   ("C-M-<right>" . nil)
                                   ("C-M-<up>"    . nil)
                                   ("C-M-<down>"  . nil)
                                   ("C-M-w"       . nil)
                                   ("C-<left>"    . nil)
                                   ("C-<right>"   . nil))
        sp-navigate-interactive-always-progress-point t))


(after! smartparens
  (add-hook! smartparens-mode 'evil-smartparens-mode))

(use-package evil-easymotion
  :config
  (define-key evilem-map "=" #'evilem-motion-next-line-first-non-blank)
  (evilem-default-keybindings "g s"))

(after! lispy
  (setq lispy-key-theme '(lispy))
  (add-to-list 'lispy-clojure-modes 'cider-repl-mode)
  (undefine-key! lispy-mode-map-lispy "[" "]" "{" "}" "M-." "C-k" "C-j")
  (lispy-set-key-theme lispy-key-theme))

(after! lispyville
  (setq lispyville-key-theme '((operators normal)
                               c-w
                               (prettify insert)
                               (atom-movement t)
                               (additional motion)
                               additional-motions
                               (commentary t)
                               mark-special))
  (lispyville-set-key-theme lispyville-key-theme)
  (add-hook! lispy-mode 'lispyville-mode)
  (map! :m "RET" 'newline-and-indent
        :mode lispy-mode
        :nvmi "DEL" 'lispy-delete-backward
        "M-<up>" 'lispyville-drag-backward
        "M-<down>" 'lispyville-drag-forward
        :mode lispyville-mode
        :nm "<" 'lispyville-beginning-of-defun
        :nm ">" 'lispyville-beginning-of-next-defun
        :i "<" nil
        :i ">" nil
        :nmi "C->" 'lispyville-end-of-defun
        :i "[" 'lispy-open-square
        :i "]" 'lispy-close-square
        :i "{" 'lispy-open-curly
        :i "}" 'lispy-close-curly
        :m "C-{" 'lispyville-previous-opening
        :m "C-}" 'lispyville-next-closing
        :m "F" 'lispyville-forward-atom-end
        :m "B" 'lispyville-backward-atom-begin
        :mi "C-f" 'lispyville-forward-sexp
        :mi "C-b" 'lispyville-backward-sexp))

(use-package! evil-matchit
  :after evil
  :init (setq evilmi–shortcut "%")
  :config (global-evil-matchit-mode 1))

(use-package! evil-snipe
  :after evil
  :init
  (setq evil-snipe-scope 'whole-visible
        evil-snipe-repeat-scope 'whole-visible
        evil-snipe-spillover-scope nil
        evil-snipe-smart-case t
        evil-snipe-tab-increment t)
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  (map! :mode (evil-snipe-override-mode evil-snipe-override-local-mode)
        :m "F" nil))

;; Swap () and [] keys
(define-key! key-translation-map
  "(" "["
  ")" "]"
  "[" "("
  "]" ")")

(defun --scroll-down-one-line ()
  (interactive)
  (forward-line 1)
  (scroll-up 1))

(defun --scroll-up-one-line ()
  (interactive)
  (forward-line -1)
  (scroll-down 1))

(use-package! elisp-mode
  :config
  (add-hook! (emacs-lisp-mode ielm-mode) 'elisp-slime-nav-mode)
  (map! :mode elisp-slime-nav-mode
        "M-." nil))

(after! magit
  (map! :mode magit-mode
        :nv "/" nil))

(map! :leader
      :desc "Kill sexp"
      "k" 'sp-kill-sexp
      ;; :desc "Kill at point"
      ;; "K" 'lispy-kill-at-point
      :desc "Wrap with ()"
      "(" 'sp-wrap-round
      :desc "Wrap with []"
      "[" 'sp-wrap-square
      :desc "Wrap with {}"
      "{" 'sp-wrap-curly
      :desc "Recenter buffer"
      "l" 'recenter-top-bottom
      :desc "Mark sexp"
      "RET" 'lispy-mark
      :desc "Mark defun"
      "S-<return>" 'er/mark-defun
      :desc "Reload doom with nix"
      "h r n" 'doom/nix-reload
      :desc "org-pomodoro"
      "C-p" 'org-pomodoro
      :desc "Large font"
      "t L" '--toggle-large-font
      :desc "Jump to cider-repl"
      "r" '--cider-goto-repl
      :desc "List workspaces"
      "0" '+workspace/display
      :desc "Switch workspace"
      "\\" '+workspace/switch-to
      :desc "Switch workspace left"
      "-" '+workspace/switch-left
      :desc "Switch workspace right"
      "=" '+workspace/switch-right
      :desc "Workspace 1"
      "1" '+workspace/switch-to-0
      :desc "Workspace 2"
      "2" '+workspace/switch-to-1
      :desc "Workspace 3"
      "3" '+workspace/switch-to-2
      :desc "Workspace 4"
      "4" '+workspace/switch-to-3
      :desc "Workspace 5"
      "5" '+workspace/switch-to-4
      :desc "Workspace 6"
      "6" '+workspace/switch-to-5
      :desc "Workspace 7"
      "7" '+workspace/switch-to-6
      :desc "Workspace 8"
      "8" '+workspace/switch-to-7
      :desc "Workspace 9"
      "9" '+workspace/switch-to-8)

(map! :m "0" 'doom/backward-to-bol-or-indent
      :m "C-a" 'doom/backward-to-bol-or-indent
      :mi "C-f" 'sp-forward-sexp
      :mi "C-b" 'sp-backward-sexp
      :m "C-e" 'evil-end-of-line
      :mi "C-y" 'yank)

(map! :m "C-o" nil
      :m "<tab>" nil
      :m "TAB" nil
      :n "M-." nil
      :g "M-<left>" nil
      :g "M-<right>" nil
      :g "C-s-<backspace>"  '+default/newline-above
      :g "C-s-k"            '+default/newline-above
      :g "C-s-<return>"     '+default/newline-below
      :g "C-s-j"            '+default/newline-below
      "<tab>" nil
      "TAB" nil
      "M-," 'pop-tag-mark
      "M-." '+lookup/definition
      "C-o" 'ace-select-window
      "C-1" 'delete-other-windows
      "C-x 1" 'delete-other-windows
      "C-2" 'delete-other-windows-vertically
      "C-x 2" 'delete-other-windows-vertically
      "C-q" 'delete-window
      "C-S-x" 'split-window-below
      "C-x x" 'split-window-below
      "C-<down>" '--scroll-down-one-line
      "C-<up>" '--scroll-up-one-line
      "C-j" '--scroll-down-one-line
      "C-k" '--scroll-up-one-line
      "C-<left>" 'previous-buffer
      "C-<right>" 'next-buffer
      ;; "C-S-<left>" '+workspace/switch-left
      ;; "C-S-<right>" '+workspace/switch-right
      "C-l" 'recenter-top-bottom
      "C-x C-k" 'kill-region
      "C-M-k" 'sp-kill-sexp
      "C-M-w" 'split-window-auto
      "C-M-f" 'toggle-frame-fullscreen
      "C-c ;" 'comment-or-uncomment-region
      "C-x \\" '--align-regexp
      "C-\\" '--align-regexp)

(eval-and-compile
  (defun symbol-suffix (sym suffix)
    (intern (concat (symbol-name sym) suffix))))

(defmacro set-mode-name (mode name)
  (let ((func-name (intern (concat "--set-mode-name--" (symbol-name mode)))))
    `(progn
       (defun ,func-name () (setq mode-name ,name))
       (add-hook ',(symbol-suffix mode "-hook") #',func-name 100)
       (when (eql major-mode ',mode)
         (,func-name)))))

(defun set-frame-fullscreen (frame active)
  (let ((current (frame-parameter (or frame (selected-frame)) 'fullscreen)))
    (when (or (and active (not current))
              (and current (not active)))
      (toggle-frame-fullscreen frame))))

(defun print-to-buffer (x)
  (princ (concat "\n" (with-output-to-string (print x))) (current-buffer)))

(defun active-minor-modes ()
  (--filter (and (boundp it) (symbol-value it)) minor-mode-list))

(defun minor-mode-active-p (minor-mode)
  (if (member minor-mode (active-minor-modes)) t nil))

(defun --body-title (body)
  (let* ((form (car (last body)))
         (full (prin1-to-string form)))
    (cond ((<= (length full) 40)
           full)
          ((and (listp form) (symbolp (car form)))
           (format "(%s ...)" (symbol-name (car form))))
          (t "<body>"))))

(defun --elapsed-seconds (start-time)
  (time-to-seconds (time-since start-time)))

(defmacro --with-elapsed-time (&rest body)
  `(let ((time-start (current-time)))
     ,@body
     (let ((elapsed (time-since time-start)))
       (time-to-seconds elapsed))))

(defun --init-time (&optional as-string)
  (let ((init-time (float-time
                    (time-subtract after-init-time before-init-time))))
    (if as-string (format "%.2fs" init-time) init-time)))
;;(--init-time t)

(defmacro with-delay (seconds &rest body)
  (declare (indent 1))
  `(let ((seconds ,seconds))
     (if (and (numberp seconds) (> seconds 0))
         (run-with-timer seconds nil (lambda () ,@body))
       (progn ,@body))))

(defun --set-paren-face-colors ()
  (let ((paren-color  (cond (t "#707070")))
        (square-color (cond (t "#bbbf40")))
        (curly-color  (cond (t "#4f8f3d"))))
    (face-spec-set 'parenthesis `((t (:foreground ,paren-color))))
    (defface square-brackets
      `((t (:foreground ,square-color)))
      "Face for displaying square brackets."
      :group 'paren-face)
    (defface curly-brackets
      `((t (:foreground ,curly-color)))
      "Face for displaying curly brackets."
      :group 'paren-face)))

(defun nxml-pretty-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max)
                             "xmllint --format -" (buffer-name) t)
    (nxml-mode)
    (indent-region (point-min) (point-max))))

(require 'align)

(defun --align-regexp (beg end regexp &optional group spacing repeat)
  "Modified version of `align-regexp`, changed to always prompt
interactively for spacing value."
  (interactive
   (append
    (list (region-beginning) (region-end))
    (if current-prefix-arg
        (list (read-string "Complex align using regexp: "
                           "\\(\\s-*\\)" 'align-regexp-history)
              (string-to-number
               (read-string
                "Parenthesis group to modify (justify if negative): " "1"))
              (string-to-number
               (read-string "Amount of spacing (or column if negative): "
                            (number-to-string align-default-spacing)))
              (y-or-n-p "Repeat throughout line? "))
      (list (concat "\\(\\s-*\\)"
                    (read-string "Align regexp: "))
            1
            (string-to-number
             (read-string "Amount of spacing (or column if negative): "
                          (number-to-string align-default-spacing)))
            nil))))
  (align-regexp beg end regexp group spacing repeat))

(defun --have-shell-command (cmd)
  (-> (shell-command-to-string (format "which %s" cmd))
      (substring-no-properties 0 1)
      (equal "/")))

(defun --root-user ()
  (equal (user-login-name) "root"))

(defun --normal-user ()
  (not (--root-user)))

;; clipboard integration
;;
;; Emacs needs to be started after Xorg or Wayland for this to work
;;
(defun xsel-paste ()
  (shell-command-to-string "xsel -ob"))
;;
(defun xsel-copy (text &optional _push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "xsel -ib" "*Messages*" "xsel" "-ib")))
      (process-send-string proc text)
      (process-send-eof proc))))
;;
(defun wl-paste ()
  (shell-command-to-string "wl-paste -n"))
;;
(defun wl-copy (text &optional _push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy")))
      (process-send-string proc text)
      (process-send-eof proc))))
;;
(defun --init-copy-paste ()
  (interactive)
  (cond ((mac?) nil)
        ((--root-user) nil)
        ((and (--wayland-available)
              (--have-shell-command "wl-copy")
              (--have-shell-command "wl-paste"))
         (setq interprogram-cut-function 'wl-copy
               interprogram-paste-function 'wl-paste))
        ((and (null window-system)
              (--window-system-available)
              (--have-shell-command "xsel"))
         (setq interprogram-cut-function 'xsel-copy
               interprogram-paste-function 'xsel-paste))))
;; (add-hook! 'after-make-frame-functions '--init-copy-paste)

(defun --session-file (filename)
  (format "%setc/workspaces/%s" doom-local-dir filename))

(defun --default-session-file ()
  (--session-file "default-session"))

(require 'ts)

(defun --today-date ()
  (ts-format "%Y-%m-%d"))

(defun --session-file-current ()
  (--session-file (ts-format "session.%Y-%m-%d_%H_%M")))

(defun --load-default-session ()
  (interactive)
  (doom/load-session (--default-session-file)))

(defun --save-current-session ()
  (interactive)
  (doom/save-session (--session-file-current))
  t)

(defun --save-default-session ()
  (interactive)
  (doom/save-session (--session-file-current))
  (doom/save-session (--default-session-file))
  t)

(defun --emacs-startup ()
  (auto-compression-mode 1)
  (--init-copy-paste)
  (when (or (graphical?) server-process)
    (--load-default-session))
  (let ((w (if (mac?) 4 4)))
    (add-to-list 'default-frame-alist `(internal-border-width . ,w))
    (set-frame-parameter nil 'internal-border-width w))
  (--kill-external-source-buffers)
  (--projectile-remove-external-projects))
(add-hook! 'emacs-startup-hook :depth 90 '--emacs-startup)

(after! tramp
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-methods '("vcsh"
                                (tramp-login-program "vcsh")
                                (tramp-login-args
                                 (("enter")
                                  ("%h")))
                                (tramp-remote-shell "/bin/sh")
                                (tramp-remote-shell-args
                                 ("-c")))))

(defun --sh-mode-hook ()
  (add-hook! 'after-save-hook :local
             'executable-make-buffer-file-executable-if-script-p)
  (company-shell-rebuild-cache))

(after! sh-script
  (set-mode-name sh-mode "Sh")
  (add-hook! sh-mode '--sh-mode-hook)
  (add-hook! (sh-mode shell-mode) 'rainbow-mode)
  (use-package! company-shell))

(after! rustic
  (setq lsp-rust-analyzer-server-display-inlay-hints nil)
  (setq lsp-rust-analyzer-server-format-inlay-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints nil)
  (setq lsp-rust-analyzer-max-inlay-hint-length 15)
  (setq lsp-rust-clippy-preference "opt-in")
  (setq lsp-rust-rustfmt-path "rustfmt")
  (setq lsp-rust-analyzer-diagnostics-disabled ["inactive-code"])
  (setq lsp-rust-analyzer-display-chaining-hints t))

(after! rainbow-mode
  (setq rainbow-html-colors t
        rainbow-html-colors-alist nil
        rainbow-ansi-colors 'auto
        rainbow-x-colors nil
        rainbow-latex-colors nil
        rainbow-r-colors nil)
  (after! hl-line
    ;; Disable hl-line-mode when rainbow-mode is enabled
    ;; because it interferes with the colors.
    (add-hook! rainbow-mode
      (hl-line-mode (if rainbow-mode -1 +1)))))

(use-package! cc-mode
  :config
  (--indent-tabs-mode 'c-mode-hook t)
  (--indent-tabs-mode 'c++-mode-hook t)
  (--indent-tabs-mode 'objc-mode-hook t)
  (--indent-tabs-mode 'java-mode-hook t))

(use-package! aggressive-indent
  :config
  (setq aggressive-indent-sit-for-time 0)
  (dolist (mode '(cider-repl-mode c-mode c++-mode objc-mode java-mode))
    (add-to-list 'aggressive-indent-excluded-modes mode))
  (after! elisp-mode
    (add-hook! emacs-lisp-mode 'aggressive-indent-mode))
  (after! markdown-mode
    (add-hook! markdown-mode 'aggressive-indent-mode))
  (after! gfm-mode
    (add-hook! gfm-mode 'aggressive-indent-mode))
  ;; conflicts with format-all-mode
  ;; (aggressive-indent-global-mode +1)
  )

(after! company
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-other-buffers nil
        company-minimum-prefix-length 1
        company-idle-delay 0.15
        company-tooltip-minimum-width 50
        company-tooltip-maximum-width 80
        company-tooltip-width-grow-only t
        company-tooltip-offset-display 'scrollbar ; 'lines
        company-box-doc-delay 0.6)
  (set-company-backend! 'text-mode
    'company-capf)
  (set-company-backend! 'prog-mode
    'company-capf
    'company-dabbrev-code
    'company-files)
  (set-company-backend! 'conf-mode
    'company-capf
    'company-dabbrev-code
    'company-files)
  (set-company-backend! 'sh-mode
    'company-capf
    'company-shell
    'company-shell-env
    'company-dabbrev-code
    'company-files)
  ;; (add-to-list 'company-transformers 'company-sort-by-occurrence)
  (use-package! company-statistics
    :disabled t
    :config
    (company-statistics-mode 1))
  (global-company-mode 1))

(setq large-file-warning-threshold (* 100 1000 1000))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook ((prog-mode . copilot-mode)
         (conf-mode . copilot-mode))
  :config
  (setq copilot-idle-delay 0
        copilot-max-char (* 1000 30))
  (map! :mode copilot-mode
        :nmi "TAB" '--copilot-show-or-accept
        :nmi "<tab>" '--copilot-show-or-accept
        :nmi "M-<tab>" '--copilot-complete-or-next
        :nmi "M-TAB" '--copilot-complete-or-next
        :nmi "S-TAB" 'copilot-accept-completion-by-line
        :nmi "<backtab>" 'copilot-accept-completion-by-line
        :nmi "C-TAB"  'copilot-accept-completion-by-word
        :nmi "C-<tab>" 'copilot-accept-completion-by-word)
  (map! :map copilot-completion-map
        :nmi "TAB" 'copilot-accept-completion
        :nmi "<tab>" 'copilot-accept-completion)
  (map! :i "C-TAB" nil
        :i "C-<tab>" nil
        :i "<backtab>" nil)
  (add-hook! (lisp-mode emacs-lisp-mode clojure-mode clojurescript-mode cider-repl-mode)
    (defun --copilot-disable-auto-complete ()
      (setq-local copilot-idle-delay 10000)))
  (--each '(--copilot-complete-or-next
            --copilot-show-or-accept)
    (add-to-list 'copilot-clear-overlay-ignore-commands it))
  (after! company
    (map! :map company-active-map
          "TAB" nil
          "<tab>" nil
          "<enter>" 'company-complete-selection
          "RET" 'company-complete-selection
          "<return>" 'company-complete-selection)))

(after! projectile
  (setq projectile-indexing-method 'hybrid
        projectile-enable-caching nil
        projectile-completion-system 'ivy)
  (map! "C-M-s" '+ivy/project-search))

(after! swiper
  (setq swiper-action-recenter t)
  (map! "C-s" 'swiper
        "C-r" 'swiper
        "C-S-S" 'swiper-all
        :m "/" 'counsel-grep-or-swiper))

(use-package! ligature)

(after! ligature
  ;; ";;" "~-" "~@" "~~" "-~"
  (->> '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
         ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
         "-<" "-<<"  "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
         "#_(" ".-" ".=" ".." "..<" "..." "?=" "??"  "/*" "/**"
         "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
         "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
         "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
         "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
         "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
         "<~" "<~~" "</" "</>" "~>" "~~>" "%%"
         ";;" ";;;")
       ;; (ligature-set-ligatures 'prog-mode)
       (ligature-set-ligatures t))
  (--each '(org-mode
            magit-status-mode
            magit-diff-mode
            magit-log-mode
            magit-stash-mode
            magit-revision-mode
            magit-section-mode)
    (add-to-list 'ligature-ignored-major-modes it))
  (global-ligature-mode 1))

(use-package! hl-todo
  :defer-incrementally t
  :config
  (global-hl-todo-mode 1))

(use-package! alert
  :defer-incrementally t
  :config
  (setq alert-default-style (if (graphical?) 'libnotify 'message)
        alert-fade-time 3))

(defun --elapsed-alert (title elapsed)
  (let ((alert-fade-time 3))
    (alert (format "Elapsed: %.3fs" elapsed) :title title)))

(defmacro --with-elapsed-time-alert (&rest body)
  `(--elapsed-alert (--body-title ',body)
    (--with-elapsed-time ,@body)))
;;(--with-elapsed-time-alert (+ 1 2))

(defun --describe-init ()
  (interactive)
  (let ((alert-fade-time (if (display-graphic-p) 3 2))
        (inhibit-message nil))
    (alert (format "Emacs started in %s" (--init-time t))
           :title (format "Emacs <%s>" (buffer-name))
           :category 'emacs-init)))

(when (graphical?)
  (add-hook! 'after-init-hook '--describe-init))

(setq-default flycheck-disabled-checkers
              '(clojure-cider-typed
                clojure-cider-kibit
                clojure-cider-eastwood
                emacs-lisp-checkdoc))

(after! flycheck
  ;; '(clojure-mode clojurec-mode clojurescript-mode groovy-mode)
  (setq flycheck-global-modes '(not org-mode js-mode)
        flycheck-indication-mode 'right-fringe ; git-gutter uses left fringe
        )
  (map! :mode flycheck-mode
        "C-c ." 'flycheck-next-error
        "C-c ," 'flycheck-previous-error)
  ;; (use-package! fringe-helper)
  ;; (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
  ;;   "...X...."
  ;;   "..XX...."
  ;;   ".XXX...."
  ;;   "XXXX...."
  ;;   ".XXX...."
  ;;   "..XX...."
  ;;   "...X....")
  (global-flycheck-mode 1))

(use-package! paren-face
  :disabled t
  :config
  (setq paren-face-regexp "[\\(\\)]")
  (global-paren-face-mode)
  (--set-paren-face-colors)
  (defconst clojure-brackets-keywords
    '(("\\[" 0 'square-brackets)
      ("\\]" 0 'square-brackets)
      ("[\\{\\}]" 0 'curly-brackets)))
  (defun --custom-paren-face-mode-hook ()
    (if paren-face-mode
        (font-lock-add-keywords nil clojure-brackets-keywords t)
      (font-lock-remove-keywords nil clojure-brackets-keywords))
    (when (called-interactively-p 'any)
      (font-lock-ensure)))
  (add-hook! 'paren-face-mode-hook '--custom-paren-face-mode-hook))

(use-package! paxedit
  :disabled t
  :config
  (setq paxedit-alignment-cleanup t
        paxedit-whitespace-cleanup t)
  (add-to-list 'emacs-lisp-mode-hook 'paxedit-mode))

(use-package! git-timemachine
  :bind ("C-M-g" . git-timemachine))

(use-package! expand-region
  :bind ("C-=" . er/expand-region))

(use-package! whitespace
  :commands whitespace-mode
  :init
  (add-hook! (prog-mode text-mode) 'whitespace-mode)
  :config
  (setq whitespace-line-column nil)
  (setq whitespace-style '(face tabs empty trailing indentation space-after-tab space-before-tab)))

(use-package! systemd
  :mode (("\\.service\\'" . systemd-mode)
         ("\\.target\\'"  . systemd-mode)))

(use-package! groovy-mode
  :mode "/Jenkinsfile"
  :config
  (setq groovy-indent-offset 2)
  (defun --groovy-mode-config ()
    (setq-local tab-width 2))
  (add-hook 'groovy-mode-hook '--groovy-mode-config))

(after! markdown-mode
  (use-package! gh-md))

(use-package! nginx-mode
  :mode ("/nginx.conf$" "\\.nginx-site\\'"))

(defun --load-org-notify ()
  (interactive)
  (when (not (featurep 'org-notify))
    (require 'org)
    (add-to-list 'load-path "~/.doom.d/org-notify")
    (require 'org-notify)
    (setq org-notify-interval 600
          org-notify-fade-time 7)))

(after! org
  (setq org-log-done 'time
        org-agenda-files '("~/todo.org"
                           "~/org/now.org"
                           "~/org/planning.org"
                           "~/org/self.org"
                           "~/org/sysrev.org")
        org-agenda-timegrid-use-ampm t)
  (map! :mode org-mode
        "C-<tab>"        nil
        "C-k"            nil
        "C-j"            nil
        "C-S-<left>"     'org-metaleft
        "C-S-<right>"    'org-metaright
        "C-S-<down>"     'org-metadown
        "C-S-<up>"       'org-metaup
        "C-S-<return>"   'org-meta-return
        ;; "M-s a"          'org-agenda-list
        ;; "M-s s"          'org-schedule
        ;; "M-s d"          'org-deadline
        )
  ;; 'org-notify-check
  ;; (use-package! org-ql)
  ;; (use-package! org-present)
  (use-package! org-projectile)
  (use-package! org-super-agenda
    :config (org-super-agenda-mode 1))
  (use-package! org-fancy-priorities)
  (use-package! org-bullets
    :disabled t
    :config
    (add-hook! 'org-mode-hook 'org-bullets-mode)
    ;; ◉ ○ ✸ ✿ ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;; ► • ★ ▸
    ;; '("◆" "◇")
    (setq org-bullets-bullet-list '("●" "◉")))
  (use-package! org-superstar
    :config
    (setq org-superstar-special-todo-items t)
    (setq org-superstar-remove-leading-stars nil))
  (defun --org-mode-hook ()
    (org-fancy-priorities-mode 1)
    (org-superstar-mode 1))
  (add-hook! 'org-mode-hook '--org-mode-hook))

(use-package! pkgbuild-mode :mode "/PKGBUILD")

(use-package! yaml-mode :mode "\\.yml\\'")

(after! cider
  (require 'tramp)
  (setq clojure-use-backtracking-indent t
        clojure-indent-style 'align-arguments  ;; 'always-align
        cider-repl-use-pretty-printing t
        cider-auto-select-error-buffer t
        cider-prompt-for-symbol nil
        cider-save-file-on-load t
        nrepl-use-ssh-fallback-for-remote-hosts t
        cider-preferred-build-tool 'shadow-cljs
        cider-default-cljs-repl 'shadow-select
        cider-shadow-default-options ":dev"
        clojure-docstring-fill-column 80
        clojure-align-forms-automatically t)
  (--each '((cider-clojure-cli-global-options . "-A:dev:test:+default")
            (clojure-indent-style quote always-align)
            (cider-test-defining-forms . ("deftest" "defspec" "deftest-browser" "deftest-etaoin")))
    (add-to-list 'safe-local-variable-values it))
  (set-mode-name clojure-mode "CLJ")
  (set-mode-name clojurescript-mode "CLJS")
  (set-mode-name clojurec-mode "CLJC")
  (add-hook! '(clojure-mode-hook clojurescript-mode-hook)
             'cider-mode)
  (add-hook! '(clojure-mode-hook clojurescript-mode-hook cider-repl-mode-hook)
             'lispy-mode)
  (define-key! 'cider-mode-map
    "M-." nil ;; 'cider-find-var
    "C-c C-k" 'cider-load-buffer
    "C-c n" 'cider-repl-set-ns)
  (define-key! 'cider-repl-mode-map
    "M-." nil)
  (use-package! flycheck-clojure
    :config (flycheck-clojure-setup))
  (use-package! flycheck-clj-kondo)
  (use-package! clj-refactor
    :config
    (setq cljr-warn-on-eval nil
          cljr-suppress-middleware-warnings t)
    (defun clj-refactor-clojure-mode-hook ()
      (clj-refactor-mode 1)
      (yas-minor-mode 1)
      (cljr-add-keybindings-with-prefix "C-'"))
    (bind-keys* :map clojure-mode-map
                ("C-<return>" . hydra-cljr-help-menu/body))
    (add-hook! '(clojure-mode-hook clojurescript-mode-hook)
               'clj-refactor-clojure-mode-hook))
  (defun --cider-reload-repl-ns ()
    (let ((ns (buffer-local-value 'cider-buffer-ns (car (cider-repls)))))
      (when ns
        (cider-nrepl-request:eval (format "(require '%s :reload)" ns)
                                  (lambda (_response) nil)))))
  (add-hook 'cider-file-loaded-hook '--cider-reload-repl-ns))

(use-package! less-css-mode
  :mode ("\\.less\\'"
         "\\.variables\\'"
         "\\.overrides\\'")
  :config (add-hook! less-css-mode 'rainbow-mode))

(use-package! jade-mode
  :mode "\\.jade\\'")

(global-auto-revert-mode t)
(transient-mark-mode t)
(delete-selection-mode t)

;; (windmove-default-keybindings '(shift))
(windmove-default-keybindings '(control meta))
;; (windmove-default-keybindings '(control shift))
(windmove-mode +1)

;; Mouse support for terminal (iterm2/alacritty)
'(when (null window-system) ;; (and (mac?) (null window-system))
   (require 'mwheel)
   (require 'mouse)
   (xterm-mouse-mode t)
   (mouse-wheel-mode t)
   (global-set-key [mouse-5] 'next-line)
   (global-set-key [mouse-4] 'previous-line))

(use-package! uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;; Make sure Emacs has the correct ssh-agent config,
;; in order to use tramp and git commands without requesting a password.
(unless (mac?)
  (if (equal (user-login-name) "root")
      (setenv "SSH_AUTH_SOCK" "/run/ssh-agent.socket")
    (setenv "SSH_AUTH_SOCK" (concat (getenv "XDG_RUNTIME_DIR") "/ssh-agent.socket"))))

;; Need to make sure emacs server daemon and emacsclient
;; are using the same path for the socket file.
;; The path is set here, and the same is set in a script
;; for starting emacsclient (/usr/local/bin/e).
;;
;; (unless (or (graphical?) (mac?))
;;   (setq server-socket-dir (format "/tmp/%s/emacs%d" (user-login-name) (user-uid))))

(require 'server)

;; Run server inside GUI process on MacOS
(when (and (gui-mac?) (null server-process))
  (server-start t t))

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(after! format-all
  (setq +format-with-lsp t
        +format-on-save-enabled-modes '(not emacs-lisp-mode
                                        sql-mode
                                        tex-mode
                                        latex-mode
                                        org-msg-edit-mode
                                        markdown-mode
                                        gfm-mode)))

(after! editorconfig
  (setq editorconfig-lisp-use-default-indent t))

(after! (lsp-mode company)
  ;; trying to fix company-mode errors from conflict with lsp-mode
  (setq lsp-completion-enable-additional-text-edit nil
        lsp-completion-default-behaviour :insert
        lsp-enable-snippet nil
        lsp-enable-links nil
        lsp-enable-symbol-highlighting nil
        lsp-response-timeout 5
        lsp-symbol-highlighting-skip-current t))

(after! (nix-mode lsp-mode)
  (setq-mode-local nix-mode +format-with-lsp t)
  (add-to-list 'aggressive-indent-excluded-modes 'nix-mode)
  (defcustom-lsp lsp-nix-nil-auto-archive nil
                 "Auto-archiving behavior which may use network."
                 :type 'lsp-json-bool
                 :group 'lsp-nix-nil
                 :lsp-path "nil.nix.flake.autoArchive"
                 :package-version '(lsp-mode . "8.0.1"))
  (defcustom-lsp lsp-nix-nil-auto-eval-inputs nil
                 "Whether to auto-eval flake inputs.
The evaluation result is used to improve completion, but may cost lots of time and/or memory."
                 :type 'boolean
                 :group 'lsp-nix-nil
                 :lsp-path "nil.nix.flake.autoEvalInputs"
                 :package-version '(lsp-mode . "8.0.1"))
  (defcustom-lsp lsp-nix-nil-nixpkgs-input-name nil
                 "The input name of nixpkgs for NixOS options evaluation.
The options hierarchy is used to improve completion, but may cost lots of time and/or memory.
If this value is `null` or is not found in the workspace flake's inputs, NixOS options are not evaluated."
                 :type 'string
                 :group 'lsp-nix-nil
                 :lsp-path "nil.nix.flake.nixpkgsInputName"
                 :package-version '(lsp-mode . "8.0.1"))
  (setq lsp-nix-rnix-server-path nil)
  (setq lsp-nix-nil-server-path "nil")
  (setq lsp-nix-nil-formatter ["nixfmt" "-w" "80"])
  (setq lsp-nix-nil-auto-archive t)
  (setq lsp-nix-nil-auto-eval-inputs nil)
  (setq lsp-nix-nil-nixpkgs-input-name "nixpkgs")
  (add-to-list 'aggressive-indent-excluded-modes 'nix-mode))

(use-package! vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")

;; List all file buffers whose path matches list of prefixes
;; Use functional programming from 'dash package to filter buffers
(defun --list-buffers-by-prefix (prefixes)
  (let ((buffers (buffer-list))
        (prefixes (-map 'expand-file-name prefixes)))
    (->> buffers
         (-filter (lambda (buf)
                    (let ((path (buffer-file-name buf)))
                      (and path
                           (-any? (lambda (prefix)
                                    (string-prefix-p prefix path))
                                  prefixes))))))))

(defvar --external-source-file-paths nil)

(--each `("/nix/store"
          "~/.maven/repository"
          "~/.cargo/registry"
          "~/.rustup"
          ,doom-emacs-dir
          ,doom-local-dir)
  (add-to-list '--external-source-file-paths (expand-file-name it)))

(defun --kill-external-source-buffers ()
  (interactive)
  (save-excursion
    (-> (--list-buffers-by-prefix --external-source-file-paths)
        (-each 'kill-buffer))))

(defun --projectile-project-external-p (project)
  (let ((path (expand-file-name (projectile-project-root project))))
    (--any? (string-prefix-p it path)
            --external-source-file-paths)))

;; List projectile projects
(defun --projectile-external-projects ()
  (->> (projectile-relevant-known-projects)
       (-filter '--projectile-project-external-p)))

(defun --projectile-remove-external-projects ()
  (interactive)
  (-> (--projectile-external-projects)
      (-each 'projectile-remove-known-project)))

(defun --cider-load-buffer-reload-repl (&optional buffer)
  (interactive)
  (cider-load-buffer buffer)
  (--cider-reload-repl-ns)
  nil)

(defun all-sesman-sessions ()
  (sesman-sessions (sesman--system) t))

(defun test-buffer-name (buf regexp &optional exclude-regexp)
  (and (string-match regexp (buffer-name buf))
       (if (null exclude-regexp) t
         (not (string-match exclude-regexp (buffer-name buf))))))

(defun match-buffer-name (regexp &optional exclude-regexp)
  (->> (buffer-list)
       (--filter (test-buffer-name it regexp exclude-regexp))))

(defun match-sesman-session (regexp &optional exclude-regexp)
  (->> (all-sesman-sessions)
       (--remove (not (test-buffer-name (cl-second it) regexp exclude-regexp)))
       (-first-item)))

(defun --cider-quit-all ()
  (interactive)
  (dolist (buf (match-buffer-name "\*cider-repl\ .*"))
    (save-excursion
      (switch-to-buffer buf)
      (cider-quit))))

(defvar --wait-on-condition-lock nil)

(defun --wait-on-condition (ready-p on-ready &optional interval timeout delay)
  (let* ((interval (or interval 0.025))
         (timeout (or timeout 5))
         (max-attempts (round (/ timeout interval)))
         (delay delay))
    (cond ((< max-attempts 1)
           nil)
          ((and (null --wait-on-condition-lock)
                (let (;; (--wait-on-condition-lock t)
                      )
                  (funcall ready-p)))
           (with-delay delay
             (let ((--wait-on-condition-lock t))
               (funcall on-ready))))
          (t (run-with-timer interval nil
                             #'--wait-on-condition
                             ready-p on-ready interval (- timeout interval) delay)))))

(defmacro wait-on-condition (test-form args &rest body)
  (declare (indent 2))
  `(--wait-on-condition
    (lambda () ,test-form)
    (lambda () ,@body)
    ,@args))
;;(wait-on-condition t nil (alert "hi"))

(defun --wait-on-buffer-text (buffer match-regexp on-ready
                                     &optional interval timeout delay)
  (let ((buffer buffer)
        (match-regexp match-regexp))
    (--wait-on-condition (lambda ()
                           (string-match-p match-regexp
                                           (with-current-buffer buffer
                                             (buffer-string))))
                         on-ready interval timeout delay)))

(defmacro wait-on-buffer-text (buffer match-regexp args &rest body)
  (declare (indent 3))
  `(let ((buffer ,buffer)
         (match-regexp ,match-regexp))
     (--wait-on-buffer-text buffer match-regexp
                            (lambda () ,@body)
                            ,@args)))
;;(wait-on-buffer-text (current-buffer) "body\) *$" (0.1 3) (alert "hi"))

(defvar --run-cider-progress nil)
(defvar --run-cider-target nil)

(defun --init-run-cider (target)
  (setq --run-cider-progress 0
        --run-cider-target target))

(defun --update-run-cider-progress ()
  (setq --run-cider-progress (+ 1 --run-cider-progress)))

(defun --run-cider-finished ()
  (= --run-cider-progress --run-cider-target))

(defun run-cider-project (project-name
                          project-file-path
                          clj-file-path
                          cljs-file-path
                          clj-test-file-path
                          figwheel-port
                          _cljs-user-ns
                          clj-repl-forms
                          cljs-repl-forms)
  (--init-run-cider 2)
  (let ((start-time (current-time))
        (project-name project-name)
        (project-file-path project-file-path)
        (clj-file-path clj-file-path)
        (cljs-file-path cljs-file-path)
        (clj-test-file-path clj-test-file-path)
        (figwheel-port figwheel-port)
        ;; (cljs-user-ns cljs-user-ns)
        (clj-repl-forms clj-repl-forms)
        (cljs-repl-forms cljs-repl-forms)
        (clj-file-buffer nil)
        (cljs-file-buffer nil))
    (cl-labels
        ((open-project
           ()
           (find-file project-file-path))
         (start-clj
           ()
           (find-file clj-file-path)
           (setq clj-file-buffer (current-buffer))
           (cider-connect
            `(:host
              "localhost"
              :port
              ,(cl-second (assoc project-name (cider-locate-running-nrepl-ports))))))
         (start-cljs
           ()
           (find-file cljs-file-path)
           (setq cljs-file-buffer (current-buffer))
           (cider-connect-cljs
            `(:host "localhost" :port ,figwheel-port :cljs-repl-type shadow-select)))
         (link-sesman-dirs
           ()
           (with-current-buffer clj-file-buffer
             (when-let ((clj-ses (match-sesman-session
                                  (format ".*cider-repl.*%s.*" project-name)
                                  (format "%d" figwheel-port))))
               (sesman-link-with-directory nil clj-ses)))
           (with-current-buffer cljs-file-buffer
             (when-let ((cljs-ses (match-sesman-session
                                   (format ".*cider-repl.*%s.*%d.*"
                                           project-name figwheel-port))))
               (sesman-link-with-directory nil cljs-ses)))
           (save-excursion
             (find-file clj-test-file-path)
             (when-let ((clj-test-ses (match-sesman-session
                                       (format ".*cider-repl.*%s.*" project-name)
                                       (format "%d" figwheel-port))))
               (sesman-link-with-directory nil clj-test-ses))
             (kill-buffer)))
         (find-clj-repl
           ()
           (cl-first (match-buffer-name
                      (format ".*cider-repl.*%s.*" project-name)
                      (format "%d" figwheel-port))))
         (find-cljs-repl
           ()
           (cl-first (match-buffer-name
                      (format ".*cider-repl.*%s.*%d.*"
                              project-name figwheel-port))))
         (have-repl-buffers
           ()
           (not (null (and (find-clj-repl) (find-cljs-repl)))))
         (show-repl-buffers
           (&optional no-init)
           (let ((clj-repl (find-clj-repl))
                 (cljs-repl (find-cljs-repl)))
             (delete-other-windows)
             (when clj-repl
               (switch-to-buffer clj-repl)
               (when no-init
                 (goto-char (point-max))))
             (when cljs-repl
               (if clj-repl
                   (switch-to-buffer-other-window cljs-repl)
                 (switch-to-buffer cljs-repl))
               (when no-init
                 (goto-char (point-max))))
             (unless no-init
               (init-repl-buffers))))
         (init-repl-buffers
           ()
           (let ((clj-repl (find-clj-repl))
                 (cljs-repl (find-cljs-repl)))
             (when clj-repl
               (wait-on-buffer-text clj-repl "user>" ()
                 (let ((ns (with-current-buffer clj-file-buffer
                             (cider-current-ns))))
                   (save-excursion
                     (switch-to-buffer clj-repl)
                     (insert (format "(in-ns '%s)" ns))
                     (cider-repl-return))
                   (wait-on-buffer-text clj-repl (format "%s> *$" ns) ()
                     (dolist (s clj-repl-forms)
                       (save-excursion
                         (switch-to-buffer clj-repl)
                         (insert (format "%s" s))
                         (cider-repl-return)))
                     (--update-run-cider-progress)))))
             (when cljs-repl
               (wait-on-buffer-text cljs-repl "cljs\.user>" (nil nil 0.025)
                 (let ((ns (with-current-buffer cljs-file-buffer
                             (cider-current-ns))))
                   (save-excursion
                     (switch-to-buffer cljs-repl)
                     (insert (format "(in-ns '%s)" ns))
                     (cider-repl-return))
                   (wait-on-buffer-text cljs-repl (format "%s> *$" ns) (nil nil 0.05)
                     (save-excursion
                       (switch-to-buffer cljs-file-buffer)
                       (--cider-load-buffer-reload-repl))
                     (dolist (s cljs-repl-forms)
                       (save-excursion
                         (switch-to-buffer cljs-repl)
                         (goto-char (point-max))
                         (insert (format "%s" s))
                         (cider-repl-return)))
                     (--update-run-cider-progress)))))
             (wait-on-condition (--run-cider-finished) ()
               (show-repl-buffers t)
               (--elapsed-alert project-name (--elapsed-seconds start-time))))))
      (--cider-quit-all)
      (open-project)
      (start-clj)
      (start-cljs)
      (link-sesman-dirs)
      (wait-on-condition (have-repl-buffers) ()
        (show-repl-buffers)))))

(defun sysrev ()
  (interactive)
  (run-cider-project
   "sysrev"
   "~/code/sysrev/project.clj"
   "~/code/sysrev/src/clj/sysrev/user.clj"
   "~/code/sysrev/src/cljs/sysrev/user.cljs"
   "~/code/sysrev/test/clj/sysrev/test/core.clj"
   7888
   "sysrev.user"
   '("(->> (q/find-count :project {}) time)")
   '("@(subscribe [:active-panel])")))

(defun --benchmark-sysrev ()
  (interactive)
  (cl-flet ((quit-all ()
              (--cider-quit-all)
              (save-excursion
                (find-file "~/code/sysrev/project.clj")
                (dolist (b (projectile-project-buffers))
                  (kill-buffer b)))))
    (quit-all)
    (with-delay 0.1 (garbage-collect))
    (with-delay 0.25 (sysrev))
    (with-delay 2.5 (quit-all))))

(defun --cider-repl-p (b)
  (eql 'cider-repl-mode (with-current-buffer b major-mode)))

(defun --cider-repl-cljs-p (b)
  (and (--cider-repl-p b)
       (not (null (string-match ".*cljs.*" (buffer-name b))))))

(defun --cider-repl-buffers ()
  (cl-remove-if-not '--cider-repl-p (buffer-list)))

(defun --cider-next-repl (current-repl)
  (->> (--cider-repl-buffers)
       (cl-remove-if (lambda (b) (equal (buffer-name b)
                                        (buffer-name current-repl))))
       (car)))

(defun --cider-goto-repl ()
  (interactive)
  (cond ((--cider-repl-p (current-buffer))
         (when-let ((b (--cider-next-repl (current-buffer))))
           (switch-to-buffer b)))
        (t
         (when-let ((b (--cider-next-repl (current-buffer))))
           (switch-to-buffer-other-window b)))))

(setq doom-modeline-buffer-file-name-style 'truncate-with-project
      doom-modeline-persp-name t
      doom-modeline-persp-icon t
      doom-modeline-buffer-encoding 'nondefault
      doom-modeline-default-eol-type 0
      doom-modeline-indent-info t)

(defvar --auto-margin nil)

(load! "auto-margin.el")

(when --auto-margin
  (dolist (hook '(window-setup-hook
                  window-size-change-functions
                  after-make-frame-functions
                  after-setting-font-hook))
    (add-hook hook 'autoset-frame-margins)))

(after! minimap
  (setq minimap-update-delay 0.1
        minimap-minimum-width 20
        minimap-width-fraction 0.15
        minimap-always-recenter nil)
  (after! ace-window
    (add-to-list 'aw-ignored-buffers "*MINIMAP*")))

(after! treemacs
  (setq +treemacs-git-mode 'deferred
        treemacs-display-in-side-window t
        treemacs-file-event-delay 500
        treemacs-silent-filewatch t
        treemacs-file-follow-delay 0.1
        treemacs-recenter-after-file-follow 'on-distance
        treemacs-recenter-distance 0.1
        treemacs-is-never-other-window t
        treemacs-show-cursor nil)
  :config
  (treemacs-follow-mode 1)
  (treemacs-project-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-git-mode 1)
  (treemacs-hide-gitignored-files-mode 1)
  (treemacs-fringe-indicator-mode 'always)
  ;; (lsp-treemacs-sync-mode 1)
  (defun --ensure-treemacs-hl-line-mode (&optional _state)
    ;; (message "running for %s" state)
    (when (treemacs-is-treemacs-window-selected?)
      (let ((dbg nil)
            (active (buffer-local-value 'hl-line-mode (window-buffer))))
        (unless active (hl-line-mode 1))
        (when dbg (message (if active
                               "hl-line-mode already active"
                             "activated hl-line-mode!"))))))
  (add-hook! 'treemacs-select-functions '--ensure-treemacs-hl-line-mode))

(defvar --ensure-treemacs-open nil)

(after! (persp-mode treemacs)
  (defun --ensure-treemacs-open (arg)
    (when (eq arg 'frame)
      ;; (unless (eql 'visible (treemacs-current-visibility)))
      ;; (if (doom-project-p) )
      (let* ((persp (get-current-persp))
             (w (frame-selected-window))
             (persp-name (persp-name persp))
             (buf (->> (persp-buffers (get-current-persp))
                       (-filter 'buffer-file-name)
                       (-find (lambda (buf)
                                (-some-> buf
                                  buffer-file-name
                                  file-name-directory
                                  projectile-project-root
                                  projectile-project-name
                                  (equal persp-name)))))))
        (if buf
            (progn
              (switch-to-buffer buf)
              (treemacs-add-and-display-current-project-exclusively))
          (treemacs))
        (select-window w))))
  (when --ensure-treemacs-open
    (add-hook! 'persp-activated-functions :append '--ensure-treemacs-open)))

(use-package! emojify
  :config
  (global-emojify-mode -1)
  (global-emojify-mode-line-mode -1)
  (custom-set-variables
   '(emojify-display-style 'image)
   '(emojify-emoji-styles '(unicode))))

(defmacro --setup-js-prettier-modes (feature-modes extra-modes)
  "Configure modes that are auto-formatted by external `prettier` tool."
  (let* ((features feature-modes)
         (modes `(,@feature-modes ,@extra-modes))
         (forms (-mapcat (lambda (mode)
                           (list `(setq-mode-local ,mode +format-with-lsp nil)
                                 `(add-to-list 'aggressive-indent-excluded-modes ',mode)))
                         modes)))
    `(after! (lsp-mode aggressive-indent)
       (-each ',features 'require)
       (after! ,features ,@forms)
       ',modes)))

(--setup-js-prettier-modes
 (web-mode typescript-mode json-mode rjsx-mode js2-mode)
 (typescript-tsx-mode))

(use-package! json-mode
  :mode "\\.json\\'" "/.prettierrc$")

(defvar --default-server-name "server")

(defun --default-session-file-path (&optional session-name)
  (concat doom-data-dir
          "workspaces/default-session"
          (unless (or (null session-name)
                      (equal session-name --default-server-name))
            (concat "." session-name))))

(defun --restore-default-session (session-name)
  "Runs `doom/load-session' using default session file for `session-name'."
  (interactive
   (list (intern
          (let ((options (->> (list server-name --default-server-name)
                              (-filter #'stringp)
                              (-distinct)
                              (-map (lambda (x)
                                      (list x "extra 1"))))))
            (ivy-read "Load session name: " options
                      :initial-input (caar options))))))
  (doom/load-session (--default-session-file-path session-name)))

(defvar --server-initialized nil)

(defun --ensure-server-initialized ()
  (when server-process
    (unless --server-initialized
      (when (equal server-name "server")
        (message (format "<%s> | Setting up emacs environment ... " server-name))
        (--restore-default-session server-name)
        (message (format "<%s> | Emacs server is ready" server-name)))
      (setq --server-initialized t))))

(add-hook! 'server-after-make-frame-hook '--ensure-server-initialized)

;; Session can be restored with buffers configured to use
;; 'git-gutter-mode instead of 'git-gutter-fringe-mode.
;; This restarts git-gutter-mode for all buffers
;; upon opening a graphical frame.
(add-hook! 'server-after-make-frame-hook '--fix-git-gutter-buffers)

;; byte-compile-warning-types

;; Local Variables:
;; byte-compile-warnings: (not free-vars constants mutate-constant)
;; End:
