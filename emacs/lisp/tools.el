(provide 'tools)

(use-package multiple-cursors
  :bind (("C-`"   . mc/edit-lines)
         ("C->"   . mc/mark-next-like-this)
         ("C-<"   . mc/mark-previous-like-this)
         ("C-c C-<". mc/mark-all-like-this)))

(use-package geiser-racket
  :commands geiser-mode)

(use-package geiser
  :custom
  (geiser-active-implementations '(racket))
  (geiser-racket-binary "/usr/bin/racket"))

(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status))

(use-package vertico
  :hook (after-init . vertico-mode)
  :custom (vertico-count 5))

(use-package savehist
  :hook (after-init . savehist-mode))

(use-package vterm
  :commands vterm
  :config
  (define-key vterm-mode-map (kbd "M-1") nil)
  (define-key vterm-mode-map (kbd "M-2") nil))

(use-package pdf-tools
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-cache-image-limit 100
        pdf-view-use-scaling nil
        pdf-view-use-imagemagick nil))

(use-package avy
  :bind ("C-<tab>" . avy-goto-char-timer))

(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . eglot-ensure)
         (go-mode . (lambda ()
                      (setq-local gofmt-command "gofmt")
                      (add-hook 'before-save-hook #'gofmt-before-save nil t))))
  :custom (gofmt-command "gofmt"))

(use-package maude-mode
  :ensure nil
  :load-path "lisp"
  :mode "\\.maude\\'")

(use-package eglot
  :hook ((go-mode . eglot-ensure)
         (c-mode  . eglot-ensure))
  :bind (:map eglot-mode-map ("C-c h" . eldoc))
  :custom
  (eglot-autoshutdown t)
  (eglot-server-programs '((go-mode . ("gopls"))))
  (eglot-ignored-server-capabilities '(:documentHighlightProvider)))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))
