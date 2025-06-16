;;; ui.el --- UI settings -*- lexical-binding: t; -*-

(set-face-attribute 'default nil :family "Source Code Pro" :height 140)

(when (display-graphic-p)
  (use-package gruber-darker-theme
    :demand t
    :config
    (load-theme 'gruber-darker t)))

(provide 'ui)
