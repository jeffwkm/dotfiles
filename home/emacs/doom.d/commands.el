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
