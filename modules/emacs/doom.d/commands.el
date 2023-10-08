;;;commands.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 'doom-lib)
(require 'doom)

(cl-declaim doom-active-minor-modes)

(defun --window-system-available () (< 0 (length (getenv "DISPLAY"))))
(defun --wayland-available () (< 0 (length (getenv "WAYLAND_DISPLAY"))))
(defun graphical? () (cl-some #'display-graphic-p (frame-list)))
(defun mac? () (eql system-type 'darwin))
(defun gui-mac-std? () (eql window-system 'ns))
(defun gui-emacs-mac? () (eql window-system 'mac))
(defun gui-mac? () (or (gui-mac-std?) (gui-emacs-mac?)))

(defun --scroll-down-one-line ()
  (interactive)
  (forward-line 1)
  (scroll-up 1))

(defun --scroll-up-one-line ()
  (interactive)
  (forward-line -1)
  (scroll-down 1))

(require 'ivy)

(defun --show-active-minor-modes ()
  (interactive)
  (ivy-read "" (doom-active-minor-modes)))

(defun -uniq-major-modes ()
  "Return list of unique major modes of all buffers."
  (-uniq (mapcar (lambda (buf)
                   (save-excursion
                     (switch-to-buffer buf)
                     major-mode))
                 (buffer-list))))

(defun kill-mode-buffers (mode)
  "Kill all buffers with major mode MODE."
  (interactive (list (intern (completing-read "Major mode: " (-uniq-major-modes) nil t))))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode mode)
        (kill-buffer buffer)))))

(require 'copilot)

(defun --copilot-complete-or-next ()
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-next-completion)
    (copilot-complete)))

(defun --copilot-show-or-accept ()
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-accept-completion)
    (copilot-complete)))

(defun --kill-auto-workspace ()
  "Delete empty auto-created workspace named #1, #2, ..."
  (interactive)
  (let ((ws (+workspace-current-name)))
    (when (and ws (= 2 (length ws)))
      (+workspace/delete ws))))

(defun --minor-mode-active-p (buffer mode)
  "Return t if minor mode MODE is active in BUFFER."
  (with-current-buffer buffer
    (--any? (eq mode it) (doom-active-minor-modes))))

(defun --buffers-with-minor-mode (mode)
  (--filter (--minor-mode-active-p it mode) (buffer-list)))

(defun --derived-mode-p (ancestor &optional mode)
  "Return t if major mode MODE is derived from ANCESTOR."
  (let ((mode (or mode major-mode)))
    (or (eq mode ancestor)
        (-when-let (derived-mode (get mode 'derived-mode-parent))
          (--derived-mode-p ancestor derived-mode)))))

(require 'git-gutter)

(declare-function +vc-gutter-init-maybe-h nil)
(declare-function +workspace/delete nil)
(declare-function +workspace-current-name nil)
(declare-function doom-active-minor-modes nil)

(defun --fix-git-gutter-buffers (&optional frame)
  "Ensure git-gutter-fringe-mode is used in all buffers if FRAME is graphical."
  (interactive)
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (--each (buffer-list)
        (with-current-buffer it
          (unless (memq major-mode git-gutter:disabled-modes)
            (git-gutter-mode +1))))
      (let ((gg-buffers (--buffers-with-minor-mode 'git-gutter-mode))
            (count 0))
        (dolist (buffer gg-buffers)
          (with-current-buffer buffer
            (git-gutter-mode -1)
            (+vc-gutter-init-maybe-h)
            (git-gutter-mode +1)
            (cl-incf count)))
        (message "Restarted git-gutter-mode in %d buffers" count)))))

(defun print-to-buffer (x)
  (princ (concat "\n" (with-output-to-string (print x))) (current-buffer)))

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
         (setq! interprogram-cut-function 'wl-copy
                interprogram-paste-function 'wl-paste))
        ((and (null window-system)
              (--window-system-available)
              (--have-shell-command "xsel"))
         (setq! interprogram-cut-function 'xsel-copy
                interprogram-paste-function 'xsel-paste))))

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
          "~/.cache"
          ,doom-emacs-dir
          ,doom-local-dir)
  (pushnew! --external-source-file-paths (expand-file-name it)))

(defun --kill-external-source-buffers ()
  (interactive)
  (save-excursion
    (-> (--list-buffers-by-prefix --external-source-file-paths)
        (-each 'kill-buffer))))

(require 'projectile)

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

(provide 'commands)
