;; Global settings
(setq
 ns-command-modifier 'meta
 ns-alternate-modifier nil
 read-file-name-completion-ignore-case 't
 select-enable-clipboard t
 gc-cons-threshold 20000000
 confirm-kill-emacs 'y-or-n-p
 user-mail-address "danielle+remax@hackcapital.com"
 create-lockfiles nil
 custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq-default
 indent-tabs-mode nil
 garbage-collection-messages t)
(prefer-coding-system 'utf-8)
(load custom-file)
(add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))
(setenv "GOPATH" "/Users/danielle/dev/go")

;; Global modes
(desktop-save-mode t)
(server-start)
(global-auto-revert-mode 1)
(add-hook 'after-revert-hook 'font-lock-fontify-buffer)
(delete-selection-mode 1)
(put 'overwrite-mode 'disabled t)

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))

;; Global key modifications
;; (global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x M-r") 'revert-buffer)
(global-set-key (kbd "M-S-SPC") 'just-one-space)
(global-set-key (kbd "C-%") 'replace-string)

;; Initialize packages
(setq use-package-always-ensure t
      use-package-verbose t)
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq-default use-package-enable-imenu-support t)
(require 'use-package)

(use-package paradox
  :defer 1
  :config
  (paradox-enable))

(use-package diminish)
(diminish 'eldoc-mode)

;; Customize builtins
(use-package frame
  :ensure nil
  :bind ("C-z" . nil))

(use-package simple
  :ensure nil
  :config
  (column-number-mode t))

(use-package ibuffer
  :ensure nil
  :bind
  ([remap list-buffers] . ibuffer))

(use-package files
  :ensure nil
  :hook (before-save . delete-trailing-whitespace)
  :custom
  (require-final-newline t)
  (backup-directory-alist
   `(("" . ,(expand-file-name (concat user-emacs-directory "backup"))))))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'post-forward))

;; Spell checking
(use-package ispell
  :ensure nil
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--run-together" "--run-together-limit=4"))
  (ispell-really-aspell t))

(use-package tool-bar
  :ensure nil
  :config (tool-bar-mode -1))

(use-package scroll-bar
  :ensure nil
  :config (scroll-bar-mode -1))

(use-package winner
  :ensure nil
  :config (winner-mode 1))

(use-package paren
  :ensure nil
  :config (show-paren-mode t))

;; More packages

(use-package sudo-edit
  :bind (:map ctl-x-map
              ("M-s" . sudo-edit)))

(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(use-package autoinsert
  :init
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)

  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (auto-insert-mode 1)
  :config
  (define-auto-insert "\\.go$" ["default-go.go" autoinsert-yas-expand])
  (define-auto-insert "\\.sql$" ["default-sql.sql" autoinsert-yas-expand])
  (define-auto-insert "\\.rb$" ["default-ruby.rb" autoinsert-yas-expand]))


;; Add AucTeX back at some point
(use-package flyspell
  :bind (:map flyspell-mouse-map
              ([down-mouse-3] . flyspell-correct-word)
              ([mouse-3] . undefined))
  :hook (prog-mode . flyspell-prog-mode))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package browse-kill-ring
  :bind ("C-M-y" . browse-kill-ring))

(use-package isearch
  :ensure nil
  :config (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char))

(use-package goto-line-preview
  :bind (("M-g g" . goto-line-preview)
         ("M-g M-g" . goto-line-preview)))

(use-package misc
  :ensure nil
  :bind ("M-Z" . zap-up-to-char))

(use-package smart-mode-line-powerline-theme)
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'light-powerline)
  (sml/setup))

(use-package dired-du)

(use-package docker
  :bind ("C-c C-d" . docker))

(use-package go-autocomplete
  :after go-mode)

(use-package auto-complete
  :config
  (ac-config-default)
  (setq ac-auto-show-menu nil
        ac-use-menu-map t)
  (define-key ac-menu-map "<up>" 'ac-next)
  (define-key ac-menu-map "<down>" 'ac-previous)
  (ac-flyspell-workaround))

(use-package flycheck
  :init (global-flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically (quote (save mode-enabled)))
  (flycheck-display-errors-delay 40)
  (flycheck-idle-change-delay 2)
  (flycheck-python-pycompile-executable "/usr/local/bin/python3"))
(setq-default flycheck-disabled-checkers '(javascript-jshint go-megacheck))

(use-package flycheck-pos-tip
  :after flycheck
  :config (flycheck-pos-tip-mode))

;; This doesn't seem to work...
(use-package flycheck-color-mode-line
  :after flycheck
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

;; Set up for React/JSX development
(use-package js2-mode
  :mode "\\.js$"
  :custom
  (js-indent-level 2)
  (js2-bounce-indent-p nil)
  (js2-include-node-externs t)
  (js2-mode-assume-strict t)
  (js2-mode-indent-ignore-first-tab nil)
  (js2-mode-show-parse-errors nil)
  (js2-strict-missing-semi-warning nil)
  (js2-global-externs '("test" "expect" "jest" "describe" "beforeEach" "afterEach" "beforeAll" "afterAll" "setTimeout" "fetch" "Blob" "Response" "Request" "Headers" "it")))

(use-package rjsx-mode
  :mode "\\.jsx$"
  :custom
  (sgml-basic-offset 2)
  (sgml-attribute-offset 2))

(use-package tern
  :hook (js2-mode . tern-mode)
  :config
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil))

(use-package tern-auto-complete
  :after (tern auto-complete-config)
  :config (tern-ac-setup))

(use-package xref-js2
  :config
  (define-key js2-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-m")
  :custom
  (js2r-always-insert-parens-around-arrow-function-params t)
  (js2r-prefered-quote-type 2))

(use-package json-mode
  :config (add-hook 'json-mode-hook (lambda () (setq-local js-indent-level 2))))

(use-package projectile
  :custom
  (projectile-project-search-path '("~/dev/"))
  (projectile-mode-line-prefix " Proj")
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :hook (after-save . magit-after-save-refresh-status)
  :hook (magit-mode . hl-line-mode)
  :config
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  :custom
  (global-magit-file-mode t)
  (magit-branch-read-upstream-first 'fallback)
  (magit-commit-ask-to-stage nil)
  (magit-stage-all-confirm nil)
  (magit-save-repository-buffers 'dontask)
  (magit-diff-refine-hunk t))

;; (use-package magit-gh-pulls
;;   :hook (magit-mode . turn-on-magit-gh-pulls))

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))

(use-package diff-hl
  :config (global-diff-hl-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package copy-as-format
  :bind (("C-c w s" . copy-as-format-slack)
         ("C-c w g" . copy-as-format-github)
         ("C-c w j" . copy-as-format-jira)))

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :config (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package eyebrowse
  :config (eyebrowse-mode t))

(use-package nameframe
  :after (eyebrowse)
  :bind ("C-x f" . nameframe-switch-frame))

(use-package nameframe-eyebrowse
  :ensure nil
  :config (nameframe-eyebrowse-mode t))

;;(use-package nameframe-projectile
;;  :ensure t
;;  :after (projectile nameframe)
;;  :config (nameframe-projectile-mode t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package wrap-region
  :hook (prog-mode . wrap-region-mode)
  :config (wrap-region-add-wrappers
           '(("`" "`" nil '(js2-mode js2-jsx-mode rjsx-mode markdown-mode))
             ("/* " " */" "*" '(js2-mode js2-jsx-mode rjsx-mode))
             ("/" "/" nil '(js2-mode javascript-mode js2-jsx-mode rjsx-mode)))))

(use-package duplicate-thing
  :bind ("C-c u" . duplicate-thing))

(use-package volatile-highlights
  :config (volatile-highlights-mode t))

(use-package indium
  :hook (js-mode . indium-interaction-mode))

(use-package org
  :bind ("C-c l" . org-store-link)
  :config (setq org-log-done t))

(use-package prettier-js
  :hook (js2-mode . prettier-js-mode))

(use-package syntactic-close
  :bind ("C-}" . syntactic-close))

(use-package yasnippet
  :bind (:map yas-minor-mode-map
              ("<tab>" . nil)
              ("TAB" . nil)
              ("C-<tab>" . yas-expand))
  :init
  (yas-global-mode 1)
  :diminish yas-minor-mode
  :config
  (setq-default ac-sources (push 'ac-source-yasnippet ac-sources)))

(use-package yasnippet-snippets)

(use-package smex
  :bind (("M-x" . smex) ("M-X" . smex-major-mode-commands)))

(use-package move-text
  :config (move-text-default-bindings))

(use-package origami
  :bind (:map origami-mode-map
              ("C-c s" . origami-open-all-nodes)
              ("C-c C-s" . origami-show-only-node)
              ("C-S-S" . origami-recursively-toggle-node))
  :hook (prog-mode . origami-mode))

(use-package sqlformat
  :hook (sql-mode . sqlformat-on-save-mode))

(use-package avy
  :bind ("C-'" . avy-goto-char))

(use-package anzu
  :config (global-anzu-mode +1))

(use-package rust-mode
  :custom (rust-format-on-save t))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package go-mode
  :config (setq gofmt-command "goimports")
  :hook (before-save . gofmt-before-save)
  :hook (go-mode . (lambda ()
                     (setq tab-width 4)
                     (setq indent-tabs-mode 1)))
  :bind ("M-." . godef-jump))

(use-package go-eldoc
  :after go-mode
  :hook (go-mode . go-eldoc-setup))

(use-package go-guru)

(use-package go-rename)

(use-package go-projectile
  :after (go-mode projectile))

(use-package gotest
  :after (go-mode)
  :bind (:map go-mode-map
              ("C-c t f" . go-test-current-file)
              ("C-c t t" . go-test-current-test)))

(use-package kubernetes
  :commands (kubernetes-overview))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;; packages with no further configuration
(use-package arduino-mode)
(use-package clojure-mode)
(use-package docker-compose-mode)
(use-package dockerfile-mode)
(use-package elm-mode)
(use-package gitignore-mode)
(use-package glsl-mode)
(use-package jade-mode)
(use-package markdown-mode)
(use-package nasm-mode)
(use-package python)
(use-package pip-requirements)
(use-package toml-mode)
(use-package yaml-mode)

(use-package flycheck-elm
  :after flycheck
  :config (flycheck-elm-setup))

;; smart openline
(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "C-o") 'prelude-smart-open-line)

;; Mark ring manipulation

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

;; Delete word rather than kill
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))
(global-set-key (kbd "M-d") 'delete-word)

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word."
  (interactive "p")
  (delete-word (- arg)))
(global-set-key (kbd "<M-backspace>") 'backward-delete-word)

(defun double-frame-width ()
  "Double the width of the current frame."
  (interactive)
  (set-frame-width (selected-frame) (* 2 (frame-width (selected-frame)))))
(global-set-key (kbd "C-c f w") 'double-frame-width)

(defun double-frame-height ()
  "Double the height of the current frame."
  (interactive)
  (set-frame-height (selected-frame) (* 2 (frame-height (selected-frame)))))
(global-set-key (kbd "C-c f h") 'double-frame-height)


(defun double-frame-dimensions ()
  "Double the width and height of the current frame."
  (interactive)
  (double-frame-width)
  (double-frame-height))
(global-set-key (kbd "C-c f d") 'double-frame-dimensions)

(defun reset-standard-frame-size ()
  "Reset to the initial frame size."
  (interactive)
  (set-frame-height (selected-frame) 36)
  (set-frame-width (selected-frame) 80))
(global-set-key (kbd "C-c f r") 'reset-standard-frame-size)

;; I don't know why this has to be at the bottom...
(define-key shell-mode-map (kbd "SPC") 'comint-magic-space)
