(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(mapc #'require '(core
                  functions
                  ui
                  tools))
