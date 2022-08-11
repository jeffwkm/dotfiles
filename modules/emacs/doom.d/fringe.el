(require 'fringe)

(defconst doom-fringe-size '16 "Default fringe width")

;;; Setting up the fringe
;; switches order of fringe and margin
(setq-default fringes-outside-margins t)

;; standardize fringe width
(fringe-mode doom-fringe-size)
;; (set-face-attribute 'fringe nil :background "#202429")
;; (set-face-attribute 'fringe nil :background nil)
(push `(left-fringe  . ,doom-fringe-size) default-frame-alist)
(push `(right-fringe . ,doom-fringe-size) default-frame-alist)

;;; Setting up git-gutter
(require 'git-gutter)
(require 'git-gutter-fringe)

;;; use larger bitmaps
(fringe-helper-define 'git-gutter-fr:added '(center t)
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX....")
(fringe-helper-define 'git-gutter-fr:modified '(center t)
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX...."
  "....XXXXXXXX....")
(fringe-helper-define 'git-gutter-fr:deleted 'center
  "X..............."
  "XX.............."
  "XXX............."
  "XXXX............"
  "XXXXX..........."
  "XXXXXX.........."
  "XXXXXXX........."
  "XXXXXXXX........"
  "XXXXXXXXX......."
  "XXXXXXXXXX......"
  "XXXXXXXXXXX....."
  "XXXXXXXXXXXX...."
  "XXXXXXXXXXXXX..."
  "XXXXXXXXXXXXXX.."
  "XXXXXXXXXXXXXXX."
  "XXXXXXXXXXXXXXXX")

;; Bootstrap
(add-hook 'text-mode 'git-gutter-mode)
(add-hook 'prog-mode 'git-gutter-mode)
(add-hook 'conf-mode 'git-gutter-mode)
