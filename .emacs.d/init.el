(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; look and feel
(load-theme 'zenburn t)
(toggle-frame-maximized)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)

(setq column-number-mode t)
(add-hook 'after-change-major-mode-hook 'fci-mode)
(setq fci-rule-column 80)
(setq fci-rule-use-dashes t)
(setq fci-rule-color "gray")

;; workaround for company mode
(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
;; workaround for company mode ends here

;; turn off storing backup files (on save) under the original name with
;; a ~ appended
(setq make-backup-files nil)
;; turn off storing auto-save files intermittently with a file name on the
;; form #file#
(setq auto-save-default nil)
;; turn off tabs for example in Markdown mode
(setq-default indent-tabs-mode nil)

(global-set-key (kbd "C-x C-b") 'ibuffer)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company fill-column-indicator zenburn-theme magit php-mode markdown-mode yaml-mode cider aggressive-indent rainbow-delimiters smartparens))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
