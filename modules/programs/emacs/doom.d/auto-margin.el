;;; auto-margin.el -*- lexical-binding: t; -*-

(require 'dash)
(require 'commands)

;; Controls max width for a centered window
(defvar auto-margin/custom-frame-width 110)
(defvar auto-margin/custom-min-margin 20)
(setq auto-margin/custom-min-margin 1000)

(defun autoset-window-margins (&optional window &rest _args)
  (let* ((min-left (--min-margin-left window))
         (w (or (and (windowp window) window)
                (selected-window)))
         (ws (window-size w t))
         (mtotal (min (- ws auto-margin/custom-frame-width 1)
                      (- (floor (/ ws 2)) 4))))
    (if (>= mtotal (* 2 auto-margin/custom-min-margin))
        (let ((m (floor (/ (- ws auto-margin/custom-frame-width 1) 2))))
          (set-window-margins w (max m min-left) m))
      (set-window-margins w (max 0 min-left) 0))))

(defun remove-frame-margins (&optional frame)
  (let ((frame (or (and (framep frame) frame)
                   (selected-frame))))
    (dolist (window (window-list frame))
      (set-window-margins window (--min-margin-left window) 0))))

(defun autoset-frame-margins (&optional frame &rest _args)
  (->> (or (and (framep frame) frame)
           (selected-frame))
       (window-list)
       (mapc 'autoset-window-margins)))

(defun autoset-window-frame-margins (window)
  (autoset-frame-margins (window-frame window)))

(provide 'auto-margin)
