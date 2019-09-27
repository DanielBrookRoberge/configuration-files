;;; init-golang.el --- Settings and packages for Go development -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setenv "GOPATH" "/Users/danielle/dev/go")

(use-package go-mode
  :config (setq gofmt-command "goimports")
  :hook (before-save . gofmt-before-save)
  :hook (go-mode . (lambda ()
                     (setq tab-width 4)
                     (setq indent-tabs-mode 1)))
  :bind
  ("M-." . godef-jump)
  ("C-c c" . go-coverage))

(use-package go-autocomplete
  :after (go-mode auto-complete))

(use-package go-dlv
  :after go-mode)

(use-package go-eldoc
  :after go-mode
  :hook (go-mode . go-eldoc-setup))

(use-package go-guru)
(use-package go-rename)

(use-package go-projectile
  :after (go-mode projectile))

(use-package gotest
  :after go-mode
  :bind (:map go-mode-map
              ("C-c t f" . go-test-current-file)
              ("C-c t t" . go-test-current-test)))

(provide 'init-golang)
;;; init-golang.el ends here
