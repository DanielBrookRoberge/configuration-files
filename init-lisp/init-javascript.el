;;; init-javascript.el --- Settings and packages for Javascript development  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

(use-package indium
  :hook (js-mode . indium-interaction-mode))

(use-package typescript-mode)

(use-package tide
  :after (typescript-mode flycheck)
  :hook (typescript-mode . tide-setup))

(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)))

(provide 'init-javascript)
;;; init-javascript.el ends here
