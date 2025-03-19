(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(electric-pair-mode 1)
(column-number-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(ido-mode 1)
(blink-cursor-mode 0)

(setq auto-revert-interval 1
      create-lockfiles nil
      echo-keystrokes 0.3
      enable-recursive-minibuffers t
      frame-inhibit-implied-resize t
      inhibit-startup-screen t
      initial-scratch-message ";; Life is clockwork. Doubt is sand.\n"
      recentf-max-saved-items 1000
      ring-bell-function 'ignore
      sentence-end-double-space nil
      custom-file (concat user-emacs-directory "custom.el")
      ido-everywhere t
      isearch-lazy-count t
      ido-create-new-buffer 'always
      ido-enable-flex-matching t
      initial-buffer-choice (lambda () (dired "~")))

(setq-default tab-width 4
              fill-column 90
              truncate-lines t
              indent-tabs-mode nil
              split-width-threshold 160
              split-height-threshold nil
              frame-resize-pixelwise t
              auto-fill-function 'do-auto-fill
              buffer-file-coding-system 'utf-8-unix)

;; (setq evil-want-keybinding nil)

;; (use-package evil
;;   :config
;;   (evil-mode 1))

;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init))

;; (with-eval-after-load 'evil
;;   (define-key evil-insert-state-map (kbd "j")
;;     (lambda ()
;;       (interactive)
;;       (let ((next-char (read-key "j")))
;;         (if (equal next-char ?j)
;;             (evil-normal-state)
;;           (insert "j" (string next-char)))))))
;; (put 'dired-find-alternate-file 'disabled nil)

(require 'dired-x)
(setq dired-omit-files "^\\.")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)
  (define-key dired-mode-map (kbd "b") 'dired-up-directory))

(tab-bar-mode 1)
(setq tab-bar-show 1
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-new-tab-choice (lambda () (dired ".")))

(defun switch-window-clockwise ()
  (interactive)
  (other-window 1))

(defun switch-window-counterclockwise ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-1") 'switch-window-counterclockwise)
(global-set-key (kbd "C-2") 'switch-window-clockwise)
(global-set-key (kbd "C-c C-1") (lambda () (interactive) (tab-move -1))) 
(global-set-key (kbd "C-c C-2") (lambda () (interactive) (tab-move 1)))   
(global-set-key (kbd "C-c n") 'tab-new)       
(global-set-key (kbd "C-c q") 'tab-close)    
(global-set-key (kbd "M-2") 'tab-next)      
(global-set-key (kbd "M-1") 'tab-previous) 
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-h") 'delete-backward-char)  
(global-set-key (kbd "M-C-h") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)


(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/"))

(setq backup-directory-alist `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms `((".*" ,emacs-autosave-directory t)))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package gruber-darker-theme
  :ensure t
  :init
  (load-theme 'gruber-darker t))

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 140)

(dolist (mode
         '(abbrev-mode
           column-number-mode
           delete-selection-mode
           dirtrack-mode
           global-auto-revert-mode
           global-so-long-mode
           recentf-mode
           show-paren-mode))
  (funcall mode 1))

(use-package magit
  :ensure t)

(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  :config
  (setq vertico-count 5))

(use-package savehist
  :ensure t
  :init
  (savehist-mode 1))

(use-package vterm
  :ensure t
  :hook (vterm-mode . (lambda ()
                        (define-key vterm-mode-map (kbd "M-1") nil)
                        (define-key vterm-mode-map (kbd "M-2") nil))))

(use-package pdf-tools
  :defer t
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("c" . (lambda ()
                       (interactive)
                       (if header-line-format
                           (setq header-line-format nil)
                         (nano-modeline-pdf-mode))))
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page))
  :hook (pdf-view-mode
         . (lambda ()
             (nano-modeline-pdf-mode)))
  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query '(".pdf")))

(use-package avy
  :ensure t
  :bind ("C-<tab>" . avy-goto-char-timer))

(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :hook (go-mode . eglot-ensure)
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package maude-mode
  :defer t
  :mode "\\.maude\\'")

(use-package eglot
  :ensure nil
  :hook ((go-mode . eglot-ensure)
         (c-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider)))

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c h") #'eldoc))

(load-file custom-file)
