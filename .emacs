;; Macintosh/Aquamacs Specific Stuff
(osx-key-mode -1)
(setq
 ns-command-modifier 'meta
 ns-alternate-modifier nil
 ns-use-mac-modifier-symbols nil
 aquamacs-scratch-file nil
 initial-major-mode 'emacs-lisp-mode
 desktop-restore-eager 50)
(one-buffer-one-frame-mode -1)
(tabbar-mode -1)
(cua-mode 0)
(setq select-enable-clipboard t)

;; General editing things
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(desktop-save-mode t)
(server-start)
(global-auto-revert-mode 1)
(tool-bar-mode 0)

;; Initialize packages
(require 'package)
(package-initialize)

;; Autocomplete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-show-menu nil
      ac-use-menu-map t)
(define-key ac-menu-map "<up>" 'ac-next)
(define-key ac-menu-map "<down>" 'ac-previous)

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(javascript-jshint))

;; Custom global key bindings
(autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
    'interactive)
(global-set-key "\M-Z" 'zap-up-to-char)
(global-set-key "\C-x\M-r" 'revert-buffer)
(global-set-key (kbd "M-S-SPC") 'just-one-space)

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
(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(with-eval-after-load 'js2-mode
  (defvar sgml-basic-offset)
  (defvar sgml-attribute-offset)
  (defvar js-indent-level)
  ;; (diminish 'js2-mode "JS")
  ;; (diminish 'js2-jsx-mode "JSX")
  (setq js-indent-level 4)
  (setq sgml-basic-offset js-indent-level
        sgml-attribute-offset js-indent-level))

(setq-default js2-global-externs '("test" "expect" "jest" "describe" "beforeEach" "afterEach" "setTimeout" "fetch" "Blob" "Response" "Request" "Headers"))

(require 'string-inflection)

(defun my-js2-mode-hook ()
  (local-set-key (kbd "C-c C-l") 'string-inflection-java-style-cycle)
  (tern-mode t)
  )

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(defun my-json-mode-hook ()
  (setq-local js-indent-level 2)
  )

(add-hook 'json-mode-hook 'my-json-mode-hook)

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

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-%") 'replace-string)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq vc-handled-backends (delq 'Git vc-handled-backends))
(remove-hook 'server-switch-hook 'magit-commit-diff)
(add-hook 'after-save-hook 'magit-after-save-refresh-status)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'wrap-region)
(add-hook 'prog-mode-hook 'wrap-region-mode)

(wrap-region-add-wrappers
 '(("`" "`" nil '(js2-mode js2-jsx-mode rjsx-mode markdown-mode))
   ("/* " " */" "*" '(js2-mode js2-jsx-mode rjsx-mode))
   ("/" "/" nil '(js2-mode javascript-mode js2-jsx-mode rjsx-mode))))

(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")

(provide '.emacs)
;;; .emacs ends here
