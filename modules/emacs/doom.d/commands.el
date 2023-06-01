;;; ../nixpkgs/home/emacs/doom.d/commands.el -*- lexical-binding: t; -*-

(defun --uniq (list)
  "Return unique elements of LIST."
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (elt list)
      (puthash elt t hash))
    (let (result)
      (maphash (lambda (key value) (push key result)) hash)
      result)))

(defun --uniq-major-modes ()
  "Return list of unique major modes of all buffers."
  (--uniq (mapcar (lambda (buf)
                    (save-excursion
                      (switch-to-buffer buf)
                      major-mode))
                  (buffer-list))))

(defun kill-mode-buffers (mode)
  "Kill all buffers with major mode MODE."
  (interactive (list (intern (completing-read "Major mode: " (--uniq-major-modes) nil t))))
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
