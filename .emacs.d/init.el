(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Look and feel
(setq solarized-distinct-fringe-background t)
(setq solarized-high-contrast-mode-line t)
(load-theme 'solarized-dark t)

(toggle-frame-maximized)
(tool-bar-mode -1)
(split-window-right)
(setq column-number-mode t)

(setq inhibit-startup-screen t)

;; Don't use tabs to indent (in Markdown mode, for example)
(setq-default indent-tabs-mode nil)
;; Require newline at the end of a file
(setq require-final-newline t)

;; Turn off storing backup files (on save) under the original name with
;; a ~ appended
(setq make-backup-files nil)
;; Turn off storing auto-save files intermittently with a file name on the
;; form #file#
(setq auto-save-default nil)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; WS
(setq whitespace-style '(face tabs empty trailing lines-tail))
(global-whitespace-mode t)

;; hs
(add-hook 'prog-mode-hook
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

;; REPL
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-repl-display-in-current-window t)

;; SP
(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'cider-repl-mode-hook #'smartparens-mode)

;; -
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

;; =>
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'cider-repl-mode-hook #'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(load "server")
(unless (server-running-p)
  (server-start))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (solarized-theme company magit markdown-mode yaml-mode cider aggressive-indent rainbow-delimiters smartparens))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
