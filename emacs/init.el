(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; evil jj
(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "j") 
    (lambda ()
      (interactive)
      (let ((next-char (read-key "j")))
        (if (equal next-char ?j)
            (evil-normal-state)
          (insert "j" (string next-char))))))
  )

;; ido mode
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)

;; shell and compilation on bottom
(setq display-buffer-alist
      '(("\\*shell\\*\\|\\*compilation\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (window-height . 0.25))))

;; always kill shell without asking
(add-hook 'shell-mode-hook
          (lambda ()
            (setq-local kill-buffer-query-functions
                        (delq 'process-kill-buffer-query-function
                              kill-buffer-query-functions))))

;; always kill compilation without asking
(add-hook 'compilation-mode-hook
          (lambda ()
            (setq-local kill-buffer-query-functions
                        (delq 'process-kill-buffer-query-function
                              kill-buffer-query-functions))))

;; switch buffers clockwise
(defun switch-buffer-clockwise ()
  "Switch to the next buffer in a clockwise manner."
  (interactive)
  (let ((windows (window-list)))
    (select-window (car (last windows)))
    (bury-buffer (current-buffer))
    (select-window (car windows))
    (other-window 1)))

;; shell shortcut
(global-set-key (kbd "C-`") 'shell)

;; use C-b for clockwise switch
(global-set-key (kbd "C-<tab>") 'switch-buffer-clockwise)

;; Add ~/.config/emacs/lisp to the load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Autoload maude-mode
(autoload 'maude-mode "maude-mode" "Major mode for editing Maude code" t)

;; Associate .maude files with maude-mode
(add-to-list 'auto-mode-alist '("\\.maude\\'" . maude-mode))


;; backup folder
(setq make-backup-files t)
(setq backup-directory-alist '(("." . "~/.emacsbackups")))
(setq backups-by-copying t)
(setq delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2)

;; font
(add-to-list 'default-frame-alist
	     '(font . "Source Code Pro-18"))

(evil-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode)

(setq-default tab-width 4
              fill-column 79
              truncate-lines t
              inhibit-splash-screen t
              auto-save-default nil
              indent-tabs-mode nil)

;; default dir when opening emacs
(setq default-directory "~/")
(add-hook 'emacs-startup-hook (lambda () (dired default-directory)))

(defun my-java-compile ()
  "Compile the current Java file."
  (interactive)
  ;; Save only the current buffer without prompting for unrelated buffers.
  (setq compilation-save-buffers-predicate 'ignore)
  (let ((compile-command (concat "javac " (buffer-file-name))))
    (compile compile-command)))

(defun my-java-run ()
  "Run the compiled Java class."
  (interactive)
  ;; Save only the current buffer without prompting for unrelated buffers.
  (let ((class-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (compile (concat "java " class-name))))

;; Keybindings for quick compile and run
(add-hook 'java-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'my-java-compile)
            (local-set-key (kbd "C-c C-r") 'my-java-run)))

;; org tab cycle
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-t") 'org-cycle))

;; omit dotfiles in dired
(require 'dired-x)

(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")     ;; Emacs autosave files
              (seq bol "." (not (any "."))) ;; Dot-files
              (seq "~" eol)             ;; Backup files
              (seq bol "CVS" eol))))    ;; CVS directorie

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)))       ;; Automatically enable omit mode in dired

(global-set-key (kbd "C-1") 'next-buffer)
(global-set-key (kbd "C-2") 'previous-buffer)
