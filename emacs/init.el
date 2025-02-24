;; -*- INTRODUCTION -*-
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)
(setq evil-want-keybinding nil)

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

;; -*- PACKAGES -*-
(use-package gruber-darker-theme)

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-continuous t) 
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
  (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-page)
  (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-page))

(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package lsp-mode
  :hook ((go-mode . lsp-deferred))
  :init
  (setq lsp-auto-guess-root t))

 (use-package lsp-ui
   :after lsp-mode
   :hook (lsp-mode . lsp-ui-mode)
   :init
   (setq lsp-ui-doc-enable t
         lsp-ui-doc-delay 2
         lsp-ui-doc-position 'at-point
         lsp-ui-sideline-show-diagnostics t
         lsp-ui-sideline-show-code-actions t))
(setq lsp-headerline-breadcrumb-enable nil)

(use-package magit)

(use-package evil
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (define-key vertico-map (kbd "C-j") 'vertico-next)
  (define-key vertico-map (kbd "C-k") 'vertico-previous))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package maude-mode)

(use-package avy)
(global-set-key (kbd "C-;") 'avy-goto-char)

(use-package maude-mode)
(autoload 'maude-mode "maude-mode" "Major mode for editing Maude code" t)
(add-to-list 'auto-mode-alist '("\\.maude\\'" . maude-mode))

;; -*- MY FUNCTIONS -*-
(add-hook 'shell-mode-hook 'my/shell-setup)
(add-hook 'evil-insert-state-entry-hook 'my/shell-move-to-prompt)

(defun my/shell-setup ()
  "Set shell buffer to read-only except for the prompt."
  (setq comint-prompt-read-only t)  ;; readonly
  (setq-local scroll-conservatively 101)  ;; autoscroll
  (add-hook 'comint-preoutput-filter-functions
            'ansi-color-apply nil t))  ;; color

(defun my/shell-move-to-prompt ()
  "Automatically move cursor to prompt line in shell mode."
  (when (derived-mode-p 'shell-mode)
    (goto-char (process-mark (get-buffer-process (current-buffer))))))

;; shell and compilation on bottom v
(setq display-buffer-alist
      '(("\\*shell\\*\\|\\*compilation\\*\\|\\*Maude\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (window-height . 0.23))))

;; always kill shell without asking
(add-hook 'shell-mode-hook
          (lambda ()
            (setq-local kill-buffer-query-functions
                        (delq 'process-kill-buffer-query-function
                              kill-buffer-query-functions))))

;; always kill compilation without asking doesnt work when exitiing emacs
(add-hook 'compilation-mode-hook
          (lambda ()
            (setq-local kill-buffer-query-functions
                        (delq 'process-kill-buffer-query-function
                              kill-buffer-query-functions))))

(defun load-config ()
  "Load the init.el config file."
  (interactive)
  (load-file (expand-file-name "~/.config/emacs/init.el")))

(defun switch-buffer-clockwise ()
  "Switch to the next buffer in a clockwise manner."
  (interactive)
  (let ((windows (window-list)))
    (select-window (car (last windows)))
    (bury-buffer (current-buffer))
    (select-window (car windows))
    (other-window 1)))

(defun my-java-compile ()
  "Compile the current Java file."
  (interactive)
  (let ((compile-command (concat "javac " (buffer-file-name))))
    (compile compile-command)))

(defun my-java-run ()
  "Run the compiled Java class."
  (interactive)
  (let ((class-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (compile (concat "java " class-name))))

;; dired split window
(defun my-dired-open-file-split-right ()
  "In dired, open the file under the cursor in a new right split."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (when file
      (split-window-right)
      (other-window 1)
      (find-file file))))

(define-key dired-mode-map (kbd "C-<return>") #'my-dired-open-file-split-right)

;; -*- KEYBINDINGS -*-
(global-set-key (kbd "C-`") 'shell)
(global-set-key (kbd "C-<tab>") 'switch-buffer-clockwise)
(global-set-key (kbd "C-c e") 'lsp-ui-doc-show)
;; (global-set-key (kbd "C-2") 'next-buffer)
;; (global-set-key (kbd "C-1") 'previous-buffer)
(add-hook 'java-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'my-java-compile)
            (local-set-key (kbd "C-c C-r") 'my-java-run)))
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; -*- CONFIGURATIONS -*-
(electric-pair-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode)
(setq transient-mark-mode nil)

(setq-default tab-width 4
              fill-column 79
              truncate-lines t
              inhibit-splash-screen t
              auto-save-default nil
              indent-tabs-mode nil)

(setq default-directory "~/"
      initial-buffer-choice (lambda () (dired default-directory)))

(set-face-attribute 'default nil :family "Source Code Pro" :height 150)
(set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'append)

(setq make-backup-files t
      backup-directory-alist '(("." . "~/.emacsbackups"))
      backups-by-copying t
      delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2)

;; org
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-t") 'org-cycle))

;; dired
(require 'dired-x)
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")
              (seq bol "." (not (any ".")))
              (seq "~" eol)
              (seq bol "CVS" eol))))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)))

(setq confirm-kill-processes nil
      message-log-max nil
      initial-scratch-message nil
      inhibit-startup-message t)

(kill-buffer "*Messages*")

;; ido mode
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)

;; jj for evil
(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "j")
    (lambda ()
      (interactive)
      (let ((next-char (read-key "j")))
        (if (equal next-char ?j)
            (evil-normal-state)
          (insert "j" (string next-char)))))))
(put 'dired-find-alternate-file 'disabled nil)

;; tabs
(tab-bar-mode 1)
(setq tab-bar-show t)  
(setq tab-bar-new-tab-choice
      (lambda ()
        (let ((buf (dired default-directory)))
          (buffer-name buf))))
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

(global-set-key (kbd "C-c C-1")
                (lambda ()
                  (interactive)
                  (tab-move -1))) 

(global-set-key (kbd "C-c C-2")
                (lambda ()
                  (interactive)
                  (tab-move 1)))   

(global-set-key (kbd "C-c n") 'tab-new)       
(global-set-key (kbd "C-c q") 'tab-close)    
(global-set-key (kbd "C-2") 'tab-next)      
(global-set-key (kbd "C-1") 'tab-previous) 


;; (use-package doc-view
  ;; :hook
  ;; ((doc-view-mode . (lambda () 
                      ;; (linum-mode -1)  
                      ;; (display-line-numbers-mode -1)
                      ;; (doc-view-continuous-scroll-mode 1)))) 
  ;; :custom
  ;; (doc-view-resolution 150) 
  ;; (doc-view-cache-directory "~/.cache/docview"))
