(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Look and feel
(setq solarized-distinct-fringe-background t)
(setq solarized-high-contrast-mode-line t)
(load-theme 'solarized-dark t)

(toggle-frame-maximized)
(set-frame-font "Source Code Pro 10")
(split-window-right)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq column-number-mode t)

(setq inhibit-startup-screen t)

;; Don't use tabs to indent (in Markdown mode, for example)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
;; Require newline at the end of a file
(setq require-final-newline t)

;; Turn off storing backup files (on save) under the original name with
;; a ~ appended
(setq make-backup-files nil)
;; Turn off storing auto-save files intermittently with a file name on the
;; form #file#
(setq auto-save-default nil)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; WS (built-in)
(setq whitespace-style '(face tabs empty trailing lines-tail))
(global-whitespace-mode t)

;; hs (build-in)
(add-hook 'prog-mode-hook
          (lambda()
            (local-set-key (kbd "C-c <right>") 'hs-show-block)
            (local-set-key (kbd "C-c <left>") 'hs-hide-block)
            (local-set-key (kbd "C-c <up>") 'hs-hide-all)
            (local-set-key (kbd "C-c <down>") 'hs-show-all)
            (hs-minor-mode t)))
;; FIXME:
(require 'sgml-mode)
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))
(add-hook 'nxml-mode-hook
          (lambda()
            (local-set-key (kbd "C-c <right>") 'hs-show-block)
            (local-set-key (kbd "C-c <left>") 'hs-hide-block)
            (local-set-key (kbd "C-c <up>") 'hs-hide-all)
            (local-set-key (kbd "C-c <down>") 'hs-show-all)
            (hs-minor-mode t)))

;; company
(add-hook 'after-init-hook 'global-company-mode)

;; cider
(setq nrepl-hide-special-buffers t)
(setq cider-save-file-on-load nil)

;; REPL
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-repl-display-in-current-window t)

;; SP
(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'cider-repl-mode-hook #'smartparens-mode)

;; =>
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'cider-repl-mode-hook #'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

;; -
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

(load "server")
(unless (server-running-p)
  (server-start))
(add-hook 'server-switch-hook #'raise-frame)

(setq tramp-default-method "ssh")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rainbow-delimiters aggressive-indent smartparens cider yaml-mode markdown-mode magit company solarized-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
