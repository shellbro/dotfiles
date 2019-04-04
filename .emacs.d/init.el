(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq inhibit-startup-screen t)

(load "server")
(add-hook 'server-switch-hook 'raise-frame)
(unless (server-running-p)
  (server-start))

(tool-bar-mode -1)
(toggle-frame-maximized)
(split-window-right)
(switch-to-buffer-other-window "*Messages*")
(switch-to-buffer-other-window "*scratch*")

(setq column-number-mode t)
(setq ring-bell-function 'ignore)
(blink-cursor-mode -1)

(setq solarized-distinct-fringe-background t)
(setq solarized-high-contrast-mode-line t)
(load-theme 'solarized-dark t)

(set-frame-font "Source Code Pro 11")

;; Require newline at the end of a file
(setq require-final-newline t)

;; Don't use tabs to indent (in Markdown mode, for example)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Turn off storing backup files (on save) under the original name with
;; a ~ appended
(setq make-backup-files nil)
;; Turn off storing auto-save files intermittently with a file name on the
;; form #file#
(setq auto-save-default nil)

(setq tramp-default-method "ssh")

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; HideShow (hs, built-in)
(add-hook 'prog-mode-hook
          (lambda()
            (local-set-key (kbd "C-c <right>") 'hs-show-block)
            (local-set-key (kbd "C-c <left>") 'hs-hide-block)
            (local-set-key (kbd "C-c <up>") 'hs-hide-all)
            (local-set-key (kbd "C-c <down>") 'hs-show-all)
            (hs-minor-mode t)))

;; Whitespace (WS, built-in)
(setq whitespace-style '(face tabs empty trailing lines-tail))
(add-hook 'after-init-hook 'global-whitespace-mode)

;; aggressive-indent (=>)
(add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
(add-hook 'cider-repl-mode-hook 'aggressive-indent-mode)
(add-hook 'clojure-mode-hook 'aggressive-indent-mode)

;; CIDER & (N)REPL (cider)
(setq cider-repl-display-in-current-window t)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-save-file-on-load nil)
(setq nrepl-hide-special-buffers t)

;; company (company)
(add-hook 'after-init-hook 'global-company-mode)

;; rainbow-delimiters (nil)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Smartparens (SP)
(add-hook 'cider-repl-mode-hook 'smartparens-mode)
(add-hook 'prog-mode-hook 'smartparens-mode)

;; TODO:
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rainbow-delimiters aggressive-indent smartparens cider yaml-mode markdown-mode neotree magit company solarized-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
