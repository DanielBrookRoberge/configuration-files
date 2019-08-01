(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
      (fringe))))
 '(fringe-mode nil nil (fringe))
 '(indicate-buffer-boundaries t)
 '(kill-do-not-save-duplicates t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode nil t)
 '(package-selected-packages
   (quote
    (flycheck-elm elm-mode sudo-edit frame restclient flycheck-rust magit-todos diminish gcmh gotest dired-du sqlformat origami toml-mode pip-requirements diff-hl goto-line-preview flycheck-color-mode-line python-mode eyebrowse ace-window kubernetes docker-compose-mode nameframe-projectile nameframe-projectile-mode nameframe go-projectile go-guru go-rename go-autocomplete go-eldoc auto-highlight-symbol misc docker browse-kill-ring anzu avy paradox move-text flycheck-pos-tip smex yasnippet-snippets volatile-highlights duplicate-thing xref-js2 yaml-mode rust-mode nasm-mode markdown-mode jade-mode go-mode glsl-mode gitignore-mode dockerfile-mode clojure-mode arduino-mode syntactic-close prettier-js indium wrap-region rainbow-delimiters expand-region use-package tern-auto-complete smart-mode-line-powerline-theme rjsx-mode projectile nvm magit-gh-pulls json-mode js2-refactor flycheck exec-path-from-shell copy-as-format)))
 '(rm-blacklist
   (quote
    (" wr" " hl-p" " AC" " Spc" " yas" " js2r" " Tern" " js-interaction" " Prettier" " guru" " (*)" " Fly" " Anzu" " VHl" " ElDoc")))
 '(rm-text-properties
   (quote
    (("\\` Ovwrt\\'"
      (quote face)
      (quote font-lock-warning-face))
     ("\\` FlyC:"
      (quote face)
      (quote font-lock-comment-face)))))
 '(sql-product (quote postgres))
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
