;;; ui.el --- UI settings -*- lexical-binding: t; -*-

(when (display-graphic-p)
  (set-face-attribute 'default nil :family "Source Code Pro" :height 140)

  (use-package gruber-darker-theme
    :demand t
    :config
    (load-theme 'gruber-darker t)))

(provide 'ui)
