(defun switch-window-clockwise ()
  (interactive)
  (other-window 1))

(defun switch-window-counterclockwise ()
  (interactive)
  (other-window -1))

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
  "Jump to the closest parenthesis, or to its match if already at one."
  (interactive)
  (cond
   ;; If at opening paren, jump forward to match
   ((looking-at-p "\\s(") (forward-sexp 1))
   ;; If just after closing paren, jump backward to match
   ((looking-back "\\s)" 1) (backward-sexp 1))
   ;; Else: jump to closest paren (open or close)
   (t
    (let* ((back (save-excursion
                   (when (re-search-backward "\\s(\\|\\s)" nil t)
                     (point))))
           (fwd (save-excursion
                  (when (re-search-forward "\\s(\\|\\s)" nil t)
                    (point)))))
      (cond
       ((and back (or (not fwd) (< (- (point) back) (- fwd (point)))))
        (goto-char back))
       (fwd
        (goto-char fwd))
       (t
        (message "No nearby parenthesis found")))))))

(defun load-config ()
  "Reload init.el and all .el files in the lisp/ folder using `load-file`."
  (interactive)
  (let* ((config-dir user-emacs-directory)
         (init-file  (expand-file-name "init.el" config-dir))
         (lisp-dir   (expand-file-name "lisp" config-dir)))
    ;; Load init.el
    (load-file init-file)

    ;; Load all .el files in lisp/ via load-file
    (when (file-directory-p lisp-dir)
      (dolist (file (directory-files lisp-dir t "\\.el$"))
        (message "Loading: %s" file)
        (load-file file)))

    (message "Config files loaded successfully.")))


(provide 'functions)
