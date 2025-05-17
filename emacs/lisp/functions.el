(provide 'functions)

(defun switch-window-clockwise ()
  (interactive)
  (other-window 1))

(defun switch-window-counterclockwise ()
  (interactive)
  (other-window -1))

(defun load-config ()
  (interactive)
  (load-file "~/.config/emacs/init.el"))

(defun smarter-move-beginning-of-line ()
  "Move to indentation, or beginning of line if already there."
  (interactive)
  (let ((pt (point)))
    (back-to-indentation)
    (when (= pt (point))
      (move-beginning-of-line 1))))

(defun go-to-emacs-folder ()
  (interactive)
  (dired user-emacs-directory))

(defun go-to-home-folder ()
  "go to ~ in dired"
  (interactive)
  (dired (expand-file-name "~")))

(defun jump-to-matching-paren ()
  (interactive)
  (cond
   ((looking-at-p "\\s(") (forward-sexp 1))
   ((looking-back "\\s)" 1) (backward-sexp 1))
   (t (message "Not at a parenthesis"))))
