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

;; Packages
;; Replace use-package with manual configurations

;; exec-path-from-shell
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; lsp-mode
(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)
(setq lsp-auto-guess-root t)

;; lsp-ui
(require 'lsp-ui)
(add-hook 'lsp-mode-hook #'lsp-ui-mode)
(setq lsp-ui-doc-enable t
      lsp-ui-doc-delay 2
      lsp-ui-doc-position 'at-point
      lsp-ui-sideline-enable nil
      lsp-ui-sideline-show-diagnostics t
      lsp-ui-sideline-show-code-actions t)

;; magit
(require 'magit)

;; evil
(require 'evil)
(evil-mode 1)

;; evil-collection
(require 'evil-collection)
(evil-collection-init)

;; vertico
(require 'vertico)
(vertico-mode)

;; savehist
(require 'savehist)
(savehist-mode)

;; maude-mode
(require 'maude-mode)

;; marginalia
(require 'marginalia)
(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)
(marginalia-mode)

;; avy
(require 'avy)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)

;; maude-mode
(autoload 'maude-mode "maude-mode" "Major mode for editing Maude code" t)
(add-to-list 'auto-mode-alist '("\\.maude\\'" . maude-mode))

;; Configure comint
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
         (window-height . 0.20))))

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

;; Custom Functions
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

;; Configurations
(electric-pair-mode 1)
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

;; Keybindings
(global-set-key (kbd "C-`") 'shell)
(global-set-key (kbd "C-<tab>") 'switch-buffer-clockwise)
(global-set-key (kbd "C-c e") 'lsp-ui-doc-show)
(global-set-key (kbd "C-2") 'next-buffer)
(global-set-key (kbd "C-1") 'previous-buffer)
(add-hook 'java-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'my-java-compile)
            (local-set-key (kbd "C-c C-r") 'my-java-run)))

;; Backup Settings
(setq make-backup-files t
      backup-directory-alist '(("." . "~/.emacsbackups"))
      backups-by-copying t
      delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2)

;; Default Directory
(setq default-directory "~/"
      initial-buffer-choice (lambda () (dired default-directory)))

;; Fonts
(add-to-list 'default-frame-alist '(font . "Source Code Pro-16"))

;; Org-Mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-t") 'org-cycle))

;; Dired Settings
(require 'dired-x)
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")
              (seq bol "." (not (any ".")))
              (seq "~" eol)
              (seq bol "CVS" eol))))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; msc
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

;; Scratch Buffer
(defvar scratch-file (expand-file-name "scratch.txt" user-emacs-directory))

(defun save-scratch-buffer ()
  "Save the content of the *scratch* buffer to a file."
  (when (get-buffer "*scratch*")
    (with-current-buffer "*scratch*"
      (write-region (point-min) (point-max) scratch-file))))

(defun load-scratch-buffer ()
  "Load the content of the *scratch* buffer from a file."
  (when (file-exists-p scratch-file)
    (with-current-buffer (get-buffer-create "*scratch*")
      (insert-file-contents scratch-file))))

(add-hook 'kill-emacs-hook 'save-scratch-buffer)
(add-hook 'emacs-startup-hook 'load-scratch-buffer)

;; JJ for Evil Mode
(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "j")
    (lambda ()
      (interactive)
      (let ((next-char (read-key "j")))
        (if (equal next-char ?j)
            (evil-normal-state)
          (insert "j" (string next-char)))))))
(put 'dired-find-alternate-file 'disabled nil)
