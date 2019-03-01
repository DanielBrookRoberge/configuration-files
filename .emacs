;; Global settings
(setq
 ns-command-modifier 'meta
 ns-alternate-modifier nil
 read-file-name-completion-ignore-case 't
 select-enable-clipboard t
 gc-cons-threshold 20000000
 confirm-kill-emacs 'y-or-n-p)
(setq-default indent-tabs-mode nil)

;; Global modes
(desktop-save-mode t)
(server-start)
(global-auto-revert-mode 1)
(add-hook 'after-revert-hook 'font-lock-fontify-buffer)
(tool-bar-mode 0)
(show-paren-mode t)
(delete-selection-mode 1)
(put 'overwrite-mode 'disabled t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))

;; Global key modifications
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x M-r") 'revert-buffer)
(global-set-key (kbd "M-S-SPC") 'just-one-space)
(global-set-key (kbd "C-%") 'replace-string)

;; Spell checking
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra" "--run-together" "--run-together-limit=4"))

;; Initialize packages
(setq use-package-always-ensure t)
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Add AucTeX back at some point
(use-package flyspell
  :bind (:map flyspell-mouse-map
              ([down-mouse-3] . flyspell-correct-word)
              ([mouse-3] . undefined))
  :hook (prog-mode . flyspell-prog-mode))

(use-package browse-kill-ring
  :bind ("C-M-y" . browse-kill-ring))

(use-package misc
  :ensure nil
  :bind ("M-Z" . zap-up-to-char))

(use-package paradox
  :custom
  (paradox-github-token ""))

(use-package smart-mode-line-powerline-theme :ensure t)
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'light-powerline)
  (sml/setup))

(use-package docker
  :bind ("C-c C-d" . docker))

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
  (flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
  (flycheck-display-errors-delay 40)
  (flycheck-idle-change-delay 2)
  (flycheck-javascript-eslint-executable "/Users/daniel/dev/mobot/node_modules/.bin/eslint"))
(setq-default flycheck-disabled-checkers '(javascript-jshint))

(use-package flycheck-pos-tip
  :config (flycheck-pos-tip-mode))


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

(use-package nvm
  :config (nvm-use (caar (last (nvm--installed-versions)))))

(defun mjs/setup-local-eslint ()
    "If ESLint found in node_modules directory - use that for flycheck.
Intended for use in PROJECTILE-AFTER-SWITCH-PROJECT-HOOK."
    (interactive)
    (let ((local-eslint (expand-file-name "./node_modules/.bin/eslint")))
      (defvar flycheck-javascript-eslint-executable)
      (setq flycheck-javascript-eslint-executable
            (and (file-exists-p local-eslint) local-eslint))))

(use-package projectile
  :hook (projectile-after-switch-project . mjs/setup-local-eslint)
  :config (projectile-mode +1)
  :pin melpa-stable)

(use-package magit
  :bind ("C-x g" . magit-status)
  :hook (after-save . magit-after-save-refresh-status)
  :hook (magit-mode . hl-line-mode)
  :config
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  :custom
  (global-magit-file-mode t)
  (magit-branch-read-upstream-first 'fallback)
  (magit-commit-ask-to-stage nil)
  (magit-stage-all-confirm nil)
  (magit-save-repository-buffers 'dontask))

(use-package magit-gh-pulls
  :hook (magit-mode . turn-on-magit-gh-pulls))

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))

(use-package copy-as-format
  :bind (("C-c w s" . copy-as-format-slack)
         ("C-c w g" . copy-as-format-github)
         ("C-c w j" . copy-as-format-jira)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

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
  :config
  (yas-global-mode 1)
  (setq-default ac-sources (push 'ac-source-yasnippet ac-sources)))

(use-package yasnippet-snippets)

(use-package smex
  :bind (("M-x" . smex) ("M-X" . smex-major-mode-commands)))

(use-package move-text
  :config (move-text-default-bindings))

(use-package avy
  :bind ("C-:" . avy-goto-char))

(use-package anzu
  :config (global-anzu-mode +1))

(use-package auto-highlight-symbol
  :config (global-auto-highlight-symbol-mode t))

(use-package rust-mode
  :custom (rust-format-on-save t))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :hook (go-mode . (lambda () (setq tab-width 4) (setq indent-tabs-mode 1))))

;; packages with no further configuration
(use-package arduino-mode)
(use-package clojure-mode)
(use-package dockerfile-mode)
(use-package gitignore-mode)
(use-package glsl-mode)
(use-package jade-mode)
(use-package markdown-mode)
(use-package nasm-mode)
(use-package python)
(use-package yaml-mode)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("" . "~/.emacs.d/backup"))))
 '(default-frame-alist
    (quote
     ((tool-bar-lines . 0)
      (menu-bar-lines . 1)
      (foreground-color . "Black")
      (background-color . "White")
      (cursor-type . box)
      (cursor-color . "Red")
      (vertical-scroll-bars . right)
      (internal-border-width . 0)
      (left-fringe . 1)
      (right-fringe)
      (fringe))))
 '(kill-do-not-save-duplicates t)
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode nil t)
 '(package-selected-packages
   (quote
    (auto-highlight-symbol misc docker browse-kill-ring anzu avy paradox move-text flycheck-pos-tip smex yasnippet-snippets volatile-highlights duplicate-thing xref-js2 yaml-mode rust-mode nasm-mode markdown-mode jade-mode go-mode glsl-mode gitignore-mode dockerfile-mode clojure-mode arduino-mode syntactic-close prettier-js indium wrap-region rainbow-delimiters expand-region use-package tern-auto-complete smart-mode-line-powerline-theme rjsx-mode projectile nvm magit-gh-pulls json-mode js2-refactor flycheck exec-path-from-shell copy-as-format)))
 '(rm-blacklist
   (quote
    (" wr" " hl-p" " AC" " Spc" " yas" " js2r" " Tern" " js-interaction" " Prettier" " guru" " (*)" " Fly" " Anzu" " VHl")))
 '(rm-text-properties
   (quote
    (("\\` Ovwrt\\'"
      (quote face)
      (quote font-lock-warning-face))
     ("\\` FlyC:"
      (quote face)
      (quote font-lock-comment-face)))))
 '(sql-product (quote postgres))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(visual-line-mode nil t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Monaco"))))
 '(autoface-default ((t (:inherit default))))
 '(completion-list-mode-default ((t (:inherit autoface-default))) t)
 '(js-mode-default ((t (:inherit prog-mode-default :height 120 :family "Monaco"))) t)
 '(js2-mode-default ((t (:inherit js-mode-default))) t)
 '(magit-popup-mode-default ((t (:inherit autoface-default))) t)
 '(magit-process-mode-default ((t (:inherit magit-mode-default))) t)
 '(magit-status-mode-default ((t (:inherit magit-mode-default))) t)
 '(minibuffer-inactive-mode-default ((t (:inherit autoface-default))) t)
 '(minibuffer-prompt ((t (:foreground "medium blue" :family "Lucida Grande"))))
 '(mode-line ((t (:background "grey85" :foreground "black" :box (:line-width -1 :color "white") :family "Lucida Grande"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey85" :foreground "grey20" :box (:line-width -2 :color "white") :slant normal))))
 '(org-mode-default ((t (:inherit autoface-default :stipple nil :strike-through nil :underline nil :slant normal :weight normal :height 120 :width normal :family "Monaco"))))
 '(sh-mode-default ((t (:inherit prog-mode-default))) t)
 '(text-mode-default ((t (:inherit autoface-default :stipple nil :strike-through nil :underline nil :slant normal :weight normal :height 130 :width normal :family "Lucida Grande"))))
 '(yaml-mode-default ((t (:inherit default))) t))
