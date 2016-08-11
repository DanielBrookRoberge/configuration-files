(osx-key-mode -1)
(setq
 ns-command-modifier 'meta
 ns-alternate-modifier nil
 ns-use-mac-modifier-symbols nil
 aquamacs-scratch-file nil
 initial-major-mode 'emacs-lisp-mode)

(one-buffer-one-frame-mode -1)

(server-start)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(desktop-save-mode t)

(tabbar-mode -1)

(global-auto-revert-mode 1)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(require 'auto-complete-config)
(ac-config-default)
(tool-bar-mode 0)
(cua-mode nil)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(javascript-jshint))

(setq ac-auto-show-menu nil
      ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "<up>" 'ac-next)
(define-key ac-menu-map "<down>" 'ac-previous)

(autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
    'interactive)
(global-set-key "\C-x\M-r" 'revert-buffer)
(global-set-key "\M-Z" 'zap-up-to-char)
(global-set-key (kbd "M-S-SPC") 'just-one-space)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-autoface-mode nil)
 '(aquamacs-customization-version-id 307 t)
 '(aquamacs-tool-bar-user-customization nil t)
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
 '(flycheck-check-syntax-automatically (quote (save)))
 '(indent-tabs-mode nil)
 '(ns-alternate-modifier nil)
 '(ns-antialias-text nil)
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(visual-line-mode nil t))
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

;; Set up projectile!
(projectile-global-mode)

;; Set up for React/JSX development
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))

(with-eval-after-load 'js2-mode
  (defvar sgml-basic-offset)
  (defvar sgml-attribute-offset)
  (defvar js-indent-level)
  ;; (diminish 'js2-mode "JS")
  ;; (diminish 'js2-jsx-mode "JSX")
  (setq js-indent-level 4)
  (setq sgml-basic-offset js-indent-level
        sgml-attribute-offset js-indent-level))

(require 'nvm)
(nvm-use (caar (last (nvm--installed-versions))))

(with-eval-after-load 'projectile
  (add-hook 'projectile-after-switch-project-hook 'mjs/setup-local-eslint))

(defun mjs/setup-local-eslint ()
    "If ESLint found in node_modules directory - use that for flycheck.
Intended for use in PROJECTILE-AFTER-SWITCH-PROJECT-HOOK."
    (interactive)
    (let ((local-eslint (expand-file-name "./node_modules/.bin/eslint")))
      (defvar flycheck-javascript-eslint-executable)
      (setq flycheck-javascript-eslint-executable
            (and (file-exists-p local-eslint) local-eslint))))

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(global-set-key (kbd "C-x g") 'magit-status)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq vc-handled-backends (delq 'Git vc-handled-backends))
(remove-hook 'server-switch-hook 'magit-commit-diff)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
