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
      org-generic-id-locations-file (convert-standard-filename
                                     (concat doom-local-dir ".org-generic-id-locations"))
      confirm-kill-processes nil
      frame-title-format '((:eval (concat (if (buffer-file-name)
                                              (abbreviate-file-name (buffer-file-name))
                                            "%b")
                                          ;; " | <Emacs>"
                                          )))
      require-final-newline t)

(setq-default tab-width 2
              fill-column 100
              byte-compile-warning-types '(not free-vars constants))

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
        "/Users/jeff/Library/Caches/Homebrew/emacs-plus@30--git/src/"))

(load! "commands.el")

(defvar --large-font nil)
(defun --configure-fonts ()
  (setq doom-font (if (mac?)
                      (font-spec :family "JetBrainsMono Nerd Font"
                                 :size 14
                                 :weight 'semibold)
                    (font-spec :family "JetBrainsMono Nerd Font"
                               :size 15
                               :weight 'semibold))
        doom-big-font nil
        doom-big-font-increment 2
        doom-font-increment 1
        doom-variable-pitch-font (if (mac?) nil
                                   (font-spec :family "Noto Sans"
                                              :size (if --large-font 17 15)))))
(defun --sync-fonts ()
  (when (graphical?)
    (set-frame-font doom-font)))
(--configure-fonts)
(add-hook! 'doom-after-reload-hook  :append '--configure-fonts '--sync-fonts)

(use-package! hl-line)

;; fixes for warnings/errors on doom init and reload
(use-package! hydra)
(use-package! ivy :config (require 'ivy-hydra))

(load! "fringe.el")

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
  (add-hook! hook (if enable-tabs '--indent-tabs-on '--indent-tabs-off)))

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

(use-package! evil-smartparens
  :config
  (after! smartparens
    (add-hook 'smartparens-mode-hook 'evil-smartparens-mode)))

(use-package evil-easymotion
  :config
  (define-key evilem-map "=" #'evilem-motion-next-line-first-non-blank)
  (evilem-default-keybindings "g s"))

(use-package! lispy
  :init
  (setq lispy-key-theme '(special lispy)))

(after! lispy
  (add-to-list 'lispy-clojure-modes 'cider-repl-mode)
  (undefine-key! lispy-mode-map-lispy "[" "]" "{" "}" "M-." "C-k" "C-j")
  (lispy-set-key-theme lispy-key-theme))

(use-package! lispyville
  :init
  (setq lispyville-key-theme '((operators normal)
                               c-w
                               (prettify insert)
                               (atom-movement t)
                               ;; slurp/barf-lispy
                               (additional motion)
                               (additional-insert motion)
                               additional-motions
                               (commentary t)
                               (additional-wrap t))))

(after! lispyville
  (lispyville-set-key-theme lispyville-key-theme)
  (map! :m "RET" 'newline-and-indent
        :mode lispy-mode
        :nvmi "DEL" 'lispy-delete-backward
        "M-<up>" 'lispyville-drag-backward
        "M-<down>" 'lispyville-drag-forward
        ;; "M-<down>" 'drag-stuff-down
        ;; "M-<up>" 'drag-stuff-up
        :mode lispyville-mode
        :nm "<" 'lispyville-beginning-of-defun
        :nm ">" 'lispyville-beginning-of-next-defun
        :nmi "C->" 'lispyville-end-of-defun
        :i "[" 'lispy-open-square
        :i "]" 'lispy-close-square
        :i "{" 'lispy-open-curly
        :i "}" 'lispy-close-curly
        :m "C-{" 'lispyville-previous-opening
        :m "C-}" 'lispyville-next-closing))

(use-package! evil-matchit
  :init (setq evilmi–shortcut "%")
  :config (global-evil-matchit-mode 1))

(use-package! evil-snipe
  :config
  (setq evil-snipe-scope 'whole-visible
        evil-snipe-repeat-scope 'whole-visible
        evil-snipe-spillover-scope nil
        evil-snipe-smart-case t
        evil-snipe-tab-increment t)
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
  (add-hook! (emacs-lisp-mode ielm-mode) 'elisp-slime-nav-mode))

(after! magit
  (map! :mode magit-mode
        ;; "0" nil
        ))

(map! :leader
      :desc "Kill sexp"
      "k" 'sp-kill-sexp
      :desc "Kill at point"
      "K" 'lispy-kill-at-point
      :desc "Wrap with ()"
      "(" 'lispyville-wrap-round
      :desc "Wrap with []"
      "[" 'lispyville-wrap-brackets
      :desc "Wrap with {}"
      "{" 'lispyville-wrap-braces
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
      :mi "C-f" 'lispyville-forward-sexp
      :mi "C-b" 'lispyville-backward-sexp
      :m "C-e" 'evil-end-of-line
      :mi "C-y" 'yank
      :m "F" 'lispyville-forward-atom-end
      :m "B" 'lispyville-backward-atom-begin)

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
  ;; (when (gui-mac?) (set-frame-fullscreen nil t))
  ;; (when (gui-mac?) (run-with-timer 0.5 nil (lambda () (toggle-frame-maximized))))
  )
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
  (setq-local tab-width 2)
  (add-hook! 'after-save-hook :local
             'executable-make-buffer-file-executable-if-script-p))

(after! sh-script
  (set-mode-name sh-mode "Sh")
  (add-hook! sh-mode '--sh-mode-hook)
  (add-hook! (sh-mode shell-mode) 'rainbow-mode))

(after! rainbow-mode
  (setq rainbow-html-colors t
        rainbow-html-colors-alist nil
        rainbow-ansi-colors 'auto
        rainbow-x-colors nil
        rainbow-latex-colors nil
        rainbow-r-colors nil))

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
  (aggressive-indent-global-mode 1))

(after! company
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-other-buffers t
        company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-tooltip-minimum-width 60
        company-tooltip-maximum-width 80
        company-tooltip-width-grow-only t
        company-tooltip-offset-display 'scrollbar ; 'lines
        company-box-doc-delay 0.3)
  (when (mac?)
    (setq company-box-tooltip-maximum-width 90))
  (set-company-backend! 'text-mode
    nil)
  (set-company-backend! 'prog-mode
    'company-capf
    'company-files)
  (set-company-backend! 'conf-mode
    'company-capf
    'company-dabbrev-code
    'company-files)
  (set-company-backend! 'nix-mode
    'company-nixos-options)
  ;; (add-to-list 'company-transformers 'company-sort-by-occurrence)
  (use-package! company-statistics
    :config
    (company-statistics-mode 1))
  (global-company-mode 1))

(setq large-file-warning-threshold (* 100 1000 1000))

;; accept completion from copilot and fallback to company
(use-package copilot
  :hook ((prog-mode . copilot-mode)
         (conf-mode . copilot-mode))
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("<backtab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("S-TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (map! :mode (prog-mode conf-mode)
        :mi "C-TAB" 'copilot-accept-completion-by-word
        :mi "C-<tab>" 'copilot-accept-completion-by-word
        :mi "TAB" 'copilot-accept-completion
        :mi "<tab>" 'copilot-accept-completion))

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

(load! "ligature.el")
(after! ligature
  (let ((all '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
               ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
               "-<" "-<<"  "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
               "#_(" ".-" ".=" ".." "..<" "..." "?=" "??"  "/*" "/**"
               "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
               "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
               "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
               "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
               "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
               "<~" "<~~" "</" "</>" "~>" "~~>" "%%"
               ;; ";;" "~-" "~@" "~~" "-~"
               )))
    ;; (ligature-set-ligatures 't all)
    (ligature-set-ligatures 'prog-mode all))
  ;; (global-ligature-mode t)
  )

(use-package! hl-todo
  :config
  (global-hl-todo-mode 1))

(use-package! alert
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
  (use-package! fringe-helper)
  (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
    "...X...."
    "..XX...."
    ".XXX...."
    "XXXX...."
    ".XXX...."
    "..XX...."
    "...X....")
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
  :config
  ;; (add-hook! 'before-save-hook 'whitespace-cleanup)
  (add-hook! (prog-mode text-mode) 'whitespace-mode)
  (setq whitespace-line-column nil)
  (setq whitespace-style '(face tabs empty trailing indentation space-after-tab space-before-tab))
  ;; render warning background for lines exceeding `fill-column`
  ;; (add-to-list 'whitespace-style 'lines-tail t #'eql)
  )

(use-package! systemd
  :mode (("\\.service\\'" . systemd-mode)
         ("\\.target\\'"  . systemd-mode)))

(use-package! groovy-mode
  :mode "/Jenkinsfile"
  :config
  (setq groovy-indent-offset 2)
  (defun --groovy-mode-config ()
    (setq-local tab-width 2))
  (add-hook 'groovy-mode-hook #'--groovy-mode-config))

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
  (use-package! org-super-agenda)
  (use-package! org-gcal :disabled t)
  (use-package! org-fancy-priorities)
  (org-fancy-priorities-mode)
  ;;(--load-org-notify)
  (use-package! org-bullets
    :config
    (add-hook! 'org-mode-hook 'org-bullets-mode)
    ;; ◉ ○ ✸ ✿ ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;; ► • ★ ▸
    ;; '("◆" "◇")
    (setq org-bullets-bullet-list '("●" "◉")))
  (use-package! org-pomodoro
    ;; https://github.com/marcinkoziej/org-pomodoro
    ;; M-x org-pomodoro (activate on org task)
    :config
    (defun --org-clock-heading () "")
    (setq org-pomodoro-length 25
          org-pomodoro-short-break-length 5
          ;; org-pomodoro-audio-player (executable-find "mpv")
          org-pomodoro-audio-player (or (executable-find "afplay")
                                        (executable-find "mpv"))
          org-pomodoro-start-sound-p t
          ;; org-clock-clocked-in-display nil
          org-clock-clocked-in-display 'mode-line
          org-clock-string-limit 12
          ;; org-clock-heading-function #'--org-clock-heading
          org-clock-heading-function nil
          ))
  (use-package! mpv
    :disabled t
    :config
    (org-link-set-parameters "mpv" :follow #'mpv-play)
    (add-hook! 'org-open-at-point-functions #'mpv-seek-to-position-at-point)
    (defun org-mpv-complete-link (&optional arg)
      (replace-regexp-in-string "file:" "mpv:"
                                (org-link-complete-file arg)
                                t t)))
  (use-package! todoist
    :disabled t
    :init
    ;; FIXME: don't put secret token in git
    (setq todoist-token "adf7ece6c35893ffc5ab2e91c6431c08bc8601c7"))
  (defun --org-mode-hook ()
    (org-fancy-priorities-mode))
  (add-hook! 'org-mode-hook '--org-mode-hook))

(use-package! pkgbuild-mode :mode "/PKGBUILD")

(use-package! yaml-mode :mode "\\.yml\\'")

(after! cider
  (require 'tramp)
  (setq clojure-use-backtracking-indent t
        clojure-indent-style 'always-align ;; 'align-arguments
        cider-repl-use-pretty-printing t
        cider-auto-select-error-buffer t
        cider-prompt-for-symbol nil
        cider-save-file-on-load t
        nrepl-use-ssh-fallback-for-remote-hosts t
        cider-preferred-build-tool 'shadow-cljs
        cider-default-cljs-repl 'shadow-select
        cider-shadow-default-options ":dev"
        clojure-docstring-fill-column 80)
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

(use-package! scala-mode
  :disabled t
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'" . scala-mode))
  :config
  (use-package! ensime
    :disabled t
    :diminish ensime-mode
    :config
    (setq ensime-startup-snapshot-notification nil)
    (setq ensime-auto-generate-config t)
    (setq ensime-typecheck-idle-interval 0.3)
    (setq ensime-completion-style 'company)
    (map! :mode scala-mode
          "C-t"     'ensime-type-at-point
          "C-M-e"   'ensime-print-errors-at-point
          "C-c ."   'ensime-forward-note
          "C-c ,"   'ensime-backward-note()
          "C-M-."   'ensime-show-uses-of-symbol-at-point)))

(use-package! haskell-mode
  :disabled t
  :mode "\\.hs\\'" "\\.hs-boot\\'" "\\.lhs\\'" "\\.lhs-boot\\'"
  :config
  (use-package! ghc)
  (use-package! ac-haskell-process
    :disabled t
    :config
    (add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
    (add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
    (add-to-list 'ac-modes 'haskell-interactive-mode))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t)
  (setq haskell-process-type 'cabal-repl))

(use-package! less-css-mode
  :mode ("\\.less\\'"
         "\\.variables\\'"
         "\\.overrides\\'")
  :config (add-hook! less-css 'rainbow-mode))

(use-package! js2-mode
  :disabled t
  :mode ("\\.js\\'"
         "\\.json\\'"
         "\\.config/waybar/config\\'")
  :config
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
  (setq js2-include-node-externs t
        js2-include-browser-externs t
        js2-strict-trailing-comma-warning nil
        js2-basic-offset 2)
  (defun --custom-js2-mode-hook ()
    (setq-local js2-basic-offset 2)
    ;;(tern-mode t)
    (when (executable-find "eslint")
      (flycheck-select-checker 'javascript-eslint)))
  (add-hook! js2-mode '--custom-js2-mode-hook)
  (add-hook js2-jsx-mode '--custom-js2-mode-hook))

(use-package! js
  :disabled t
  :mode (("\\.js\\'"                      . js-mode)
         ("\\.json\\'"                    . js-mode)
         ("\\.config/waybar/config\\'"    . js-mode))
  :init (setq js-indent-level 2))

(use-package! web-mode
  :disabled t
  :mode "\\.jsx\\'" ;; ("\\.js\\'" "\\.jsx\\'" "\\.json\\'")
  :config
  (use-package! tern :disabled t)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (add-to-list 'flycheck-disabled-checkers 'json-jsonlist)
  (defun --custom-web-mode-hook ()
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (flycheck-mode 1)
    ;; (tern-mode t)
    (when (executable-find "eslint")
      (flycheck-select-checker 'javascript-eslint)))
  (add-hook! web-mode '--custom-web-mode-hook))

(use-package! jade-mode
  :disabled t
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

(use-package! recentf
  :disabled t
  :config
  (setq recentf-save-file (expand-file-name "recentf" --savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(after! nix-mode
  (use-package! nix-sandbox)
  (use-package! nix-buffer)
  (use-package! nix-update)
  (use-package! company-nixos-options)
  (defun --nix-mode-hook ()
    (setq-local tab-width 2))
  (add-hook! nix-mode '--nix-mode-hook))

(defun --kill-auto-workspace ()
  "Delete empty auto-created workspace named #1, #2, ..."
  (let ((ws (+workspace-current-name)))
    (when (and ws (= 2 (length ws)))
      (+workspace/delete ws))))

(defun --cider-load-buffer-reload-repl (&optional buffer)
  (interactive)
  (let ((result (if buffer
                    (cider-load-buffer buffer)
                  (cider-load-buffer))))
    (--cider-reload-repl-ns)
    result))

(defun all-sesman-sessions ()
  (sesman-sessions (sesman--system) t))

(defun test-buffer-name (buf regexp &optional exclude-regexp)
  (and (string-match regexp (buffer-name buf))
       (if (null exclude-regexp) t
         (not (string-match exclude-regexp (buffer-name buf))))))

(defun match-buffer-name (regexp &optional exclude-regexp)
  (--filter (test-buffer-name it regexp exclude-regexp)
            (buffer-list)))

(defun match-sesman-session (regexp &optional exclude-regexp)
  (->> (all-sesman-sessions)
       (--remove (not (test-buffer-name (cl-second it) regexp exclude-regexp)))
       (cl-first)))

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
  `(--wait-on-condition (lambda () ,test-form)
                        (lambda () ,@body)
                        ,@args))
;;(wait-on-condition t nil (alert "hi"))

(defun --wait-on-buffer-text (buffer match-regexp on-ready
                                     &optional interval timeout delay)
  (let ((buffer buffer)
        (match-regexp match-regexp)
        ;; (on-ready on-ready)
        ;; (interval interval)
        ;; (timeout timeout)
        ;; (delay delay)
        ;; (delay (or delay 0.02))
        )
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

(setq doom-modeline-buffer-file-name-style 'truncate-upto-project
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

(use-package! treemacs
  :defer t
  :init
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
  ;; (treemacs)
  (treemacs-follow-mode 1)
  ;; (treemacs-project-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-git-mode 1)
  (treemacs-hide-gitignored-files-mode 1)
  (treemacs-fringe-indicator-mode 'always)
  (defun --ensure-treemacs-hl-line-mode (&optional state)
    ;; (message "running for %s" state)
    (when (treemacs-is-treemacs-window-selected?)
      (let ((dbg nil)
            (active (buffer-local-value 'hl-line-mode (window-buffer))))
        (unless active (hl-line-mode 1))
        (when dbg (message (if active
                               "hl-line-mode already active"
                             "activated hl-line-mode!"))))))
  (add-hook! 'treemacs-select-functions '--ensure-treemacs-hl-line-mode))

(after! persp-mode
  (after! treemacs
    (defun --ensure-treemacs-open (arg)
      (when (eq arg 'frame)
        (unless (eql 'visible (treemacs-current-visibility))
          (if (doom-project-p)
              (treemacs-add-and-display-current-project)
            (treemacs)))))
    ;; (add-to-list 'persp-activated-functions #'--ensure-treemacs-open t)
    ))
;; persp-activated-functions
;; (setf persp-activated-functions (delete 'treemacs persp-activated-functions))
;; (setf persp-activated-functions (delete '+treemacs/toggle persp-activated-functions))

(after! emojify
  (global-emojify-mode -1))

(defun --darwin-rebuild-switch ()
  (interactive)
  (message "[darwin-rebuild switch] running ...")
  (call-process-shell-command "darwin-rebuild switch")
  (message "[darwin-rebuild switch] finished"))
(defun --home-manager-switch ()
  (interactive)
  (message "[home-manager switch] running ...")
  (call-process-shell-command "home-manager switch")
  (message "[home-manager switch] finished"))
(defun doom/nix-reload ()
  (interactive)
  (--home-manager-switch)
  (doom/reload))

(use-package! webkit-color-picker
  :disabled t
  :bind (("C-c C-p" . webkit-color-picker-show)))

(defvar --default-server-name "server")

(defun --default-session-file-path (&optional session-name)
  (concat doom-etc-dir
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

;; (defun --projectile-project-name-function ()
;;   "Wraps default implementation for #'projectile-project-name-function
;;    to add custom rules for renaming some projects."
;;   (let ((path (projectile-project-root))
;;         (default-name (projectile-default-project-name path)))
;;     (cond ((equal path (format "%s/.doom.d/" (getenv "HOME")))
;;            ))))
;; (setq projectile-project-name-function '--projectile-project-name-function)
