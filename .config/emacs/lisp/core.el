(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
(add-to-list 'exec-path (expand-file-name "~/go/bin"))

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(electric-pair-mode 1)
(column-number-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode 0)
(global-subword-mode 1)

(setq use-package-always-ensure t
      auto-revert-interval 1
      create-lockfiles nil
      echo-keystrokes 0.3
      kill-whole-line t
      enable-recursive-minibuffers t
      frame-inhibit-implied-resize t
      inhibit-startup-screen t
      initial-scratch-message ";; Life is clockwork. Doubt is sand.\n"
      recentf-max-saved-items 1000
      ring-bell-function 'ignore
      sentence-end-double-space nil
      dired-dwim-target t
      isearch-lazy-count t
      initial-buffer-choice (lambda () (dired "~"))
      shell-file-name "/usr/bin/bash"
      explicit-shell-file-name shell-file-name
      explicit-bash-args '("--login" "-i")
      shell-command-switch "-lc")

(setq-default tab-width 4
              fill-column 90
              truncate-lines t
              indent-tabs-mode nil
              split-width-threshold 160
              split-height-threshold nil
              frame-resize-pixelwise t
              auto-fill-function 'do-auto-fill
              buffer-file-coding-system 'utf-8-unix)

(dolist (mode '(abbrev-mode
                column-number-mode
                delete-selection-mode
                dirtrack-mode
                global-auto-revert-mode
                global-so-long-mode
                recentf-mode
                show-paren-mode))
  (funcall mode 1))

(use-package emacs
  :ensure nil
  :bind (("C-1"       . switch-window-counterclockwise)
         ("C-2"       . switch-window-clockwise)
         ("C-c C-1"   . (lambda () (interactive) (tab-move -1)))
         ("C-c C-2"   . (lambda () (interactive) (tab-move 1)))
         ("C-c n"     . tab-new)
         ("C-c q"     . tab-close)
         ("M-2"       . tab-next)
         ("M-1"       . tab-previous)
         ("<C-up>"    . shrink-window)
         ("<C-down>"  . enlarge-window)
         ("<C-left>"  . shrink-window-horizontally)
         ("<C-right>" . enlarge-window-horizontally)
         ("C-w"       . backward-kill-word)
         ("C-h"       . delete-backward-char)
         ("M-C-h"     . backward-kill-word)
         ("M-n"       . forward-paragraph)
         ("M-p"       . backward-paragraph)
         ("<f1>"      . vterm)
         ("C-v"       . scroll-down-command)
         ("M-v"       . scroll-up-command)
         ("C-a"       . smarter-move-beginning-of-line)
         ("C-x C-r"   . #'recentf-open-files)
         ("M-m"       . move-beginning-of-line)
         ("C-q"       . jump-to-matching-paren)
         ("C-z"       . kill-region)))

(use-package dired-x
  :ensure nil
  :custom
  (dired-omit-files "^\\.")
  :hook
  (dired-mode . dired-omit-mode))

(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
        ("<backspace>" . dired-up-directory)
        ("b"            . dired-up-directory)))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-show 1)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show   nil)
  (tab-bar-new-tab-choice (lambda () (dired ".")))
  :config
  (tab-bar-mode 1))

(let* ((backup-dir   (expand-file-name "backups/"   user-emacs-directory))
       (autosave-dir (expand-file-name "autosaves/" user-emacs-directory)))
  (make-directory backup-dir t)
  (make-directory autosave-dir t)
  (setq backup-directory-alist
        `((".*" . ,backup-dir))
        auto-save-file-name-transforms
        `((".*" ,autosave-dir t))))

(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 2000)
  (recentf-mode 1))

(provide 'core)

