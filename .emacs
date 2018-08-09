;; Macintosh/Aquamacs Specific Stuff
;; ____________________________________________________________________________
;; Aquamacs custom-file warning:
;; Warning: After loading this .emacs file, Aquamacs will also load
;; customizations from `custom-file' (customizations.el). Any settings there
;; will override those made here.
;; Consider moving your startup settings to the Preferences.el file, which
;; is loaded after `custom-file':
;; ~/Library/Preferences/Aquamacs Emacs/Preferences
;; _____________________________________________________________________________
(osx-key-mode -1)
(setq
 ns-command-modifier 'meta
 ns-alternate-modifier nil
 ns-use-mac-modifier-symbols nil
 aquamacs-scratch-file nil
 initial-major-mode 'emacs-lisp-mode)
(one-buffer-one-frame-mode -1)
(tabbar-mode -1)
(cua-mode 0)
(setq select-enable-clipboard t)
(setq gc-cons-threshold 20000000)
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default indent-tabs-mode nil)

;; General editing things
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(desktop-save-mode t)
(server-start)
(global-auto-revert-mode 1)
(tool-bar-mode 0)
(global-unset-key (kbd "C-z"))
(show-paren-mode t)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Initialize packages
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(setq sml/no-confirm-load-theme t)
(setq sml/theme 'light-powerline)
(sml/setup)

;; Autocomplete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-show-menu nil
      ac-use-menu-map t)
(define-key ac-menu-map "<up>" 'ac-next)
(define-key ac-menu-map "<down>" 'ac-previous)
(ac-flyspell-workaround)

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(setq-default flycheck-disabled-checkers '(javascript-jshint))

;; Beacon mode
(beacon-mode 1)

;; Custom global key bindings
(autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
    'interactive)
(global-set-key "\M-Z" 'zap-up-to-char)
(global-set-key "\C-x\M-r" 'revert-buffer)
(global-set-key (kbd "M-S-SPC") 'just-one-space)
(global-set-key (kbd "C-%") 'replace-string)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-mode-default ((t (:inherit prog-mode-default :height 120 :family "Monaco"))) t))

(add-hook 'html-mode-hook
        (lambda ()
          ;; Default indentation is usually 2 spaces, changing to 4.
          (set (make-local-variable 'sgml-basic-offset) 4)))

(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
;; If there's a problem with selection, the var is mouse-save-then-kill

;; Set up for React/JSX development
(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(with-eval-after-load 'js2-mode
  (defvar sgml-basic-offset)
  (defvar sgml-attribute-offset)
  (defvar js-indent-level)
  (setq js-indent-level 2)
  (setq sgml-basic-offset js-indent-level
        sgml-attribute-offset js-indent-level))

(setq-default js2-global-externs '("test" "expect" "jest" "describe" "beforeEach" "afterEach" "beforeAll" "afterAll" "setTimeout" "fetch" "Blob" "Response" "Request" "Headers" "it"))

(defun my-js2-mode-hook () (tern-mode t))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(defun my-json-mode-hook ()
  (setq-local js-indent-level 2))

(add-hook 'json-mode-hook 'my-json-mode-hook)

(require 'nvm)
(nvm-use (caar (last (nvm--installed-versions))))

(defun mjs/setup-local-eslint ()
    "If ESLint found in node_modules directory - use that for flycheck.
Intended for use in PROJECTILE-AFTER-SWITCH-PROJECT-HOOK."
    (interactive)
    (let ((local-eslint (expand-file-name "./node_modules/.bin/eslint")))
      (defvar flycheck-javascript-eslint-executable)
      (setq flycheck-javascript-eslint-executable
            (and (file-exists-p local-eslint) local-eslint))))

;; Set up projectile!
(use-package projectile
  :hook (projectile-after-switch-project . mjs/setup-local-eslint)
  :config (projectile-mode +1)
  :ensure t
  :pin melpa-stable)

(use-package magit
  :bind ("C-x g" . magit-status)
  :hook (after-save . magit-after-save-refresh-status)
  :config
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  :ensure t)

(use-package magit-gh-pulls
  :hook (magit-mode . turn-on-magit-gh-pulls)
  :requires magit
  :ensure t)

(use-package copy-as-format
  :ensure t
  :bind (("C-c w s" . copy-as-format-slack)
         ("C-c w g" . copy-as-format-github)
         ("C-c w j" . copy-as-format-jira)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :ensure t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :ensure t)

(use-package wrap-region
  :ensure t
  :hook (prog-mode . wrap-region-mode)
  :config (wrap-region-add-wrappers
           '(("`" "`" nil '(js2-mode js2-jsx-mode rjsx-mode markdown-mode))
             ("/* " " */" "*" '(js2-mode js2-jsx-mode rjsx-mode))
             ("/" "/" nil '(js2-mode javascript-mode js2-jsx-mode rjsx-mode)))))

(use-package js2-refactor
  :ensure t
  :hook (js2-mode . js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package indium
  :ensure t
  :hook (js-mode . indium-interaction-mode))

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(setq org-log-done t)

(use-package prettier-js
  :hook (js2-mode . prettier-js-mode)
  :ensure t)

(use-package syntactic-close
  :bind ("C-}" . syntactic-close)
  :ensure t)

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(provide '.emacs)
;;; .emacs ends here
