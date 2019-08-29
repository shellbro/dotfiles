(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq column-number-mode t)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(setq solarized-distinct-fringe-background t)
(setq solarized-high-contrast-mode-line t)
(load-theme 'solarized-dark t)
(set-frame-font "Source Code Pro 11")

(toggle-frame-maximized)
(split-window-right)
(switch-to-buffer-other-window "*Messages*")
(switch-to-buffer-other-window "*scratch*")

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(setq tramp-default-method "ssh")

;; Require newline at the end of a file
(setq require-final-newline t)

;; Don't use tabs to indent (in Markdown mode, for example)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Turn off storing auto-save files intermittently with a file name on the
;; form #file#
(setq auto-save-default nil)
;; Turn off storing backup files (on save) under the original name with
;; a ~ appended
(setq make-backup-files nil)

(require 'use-package)

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(use-package cider
  :ensure t
  :config
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq cider-save-file-on-load nil)
  (setq nrepl-hide-special-buffers t))

(use-package smartparens
  :ensure t
  :hook ((prog-mode cider-repl-mode) . smartparens-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode cider-repl-mode) . rainbow-delimiters-mode))

(use-package aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode clojure-mode cider-repl-mode) .
         aggressive-indent-mode))

(use-package whitespace
  :hook (after-init . global-whitespace-mode)
  :config
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package hideshow
  :init
  (add-hook 'prog-mode-hook
            (lambda()
              (local-set-key (kbd "C-c <right>") 'hs-show-block)
              (local-set-key (kbd "C-c <left>") 'hs-hide-block)
              (local-set-key (kbd "C-c <up>") 'hs-hide-all)
              (local-set-key (kbd "C-c <down>") 'hs-show-all)
              (hs-minor-mode t))))

(use-package neotree
  :ensure t
  :hook (after-init . neotree-toggle)
  :bind ([f8] . neotree-refresh)
  :config
  (setq neo-theme 'icons)
  (setq neo-autorefresh nil))

(use-package server
  :init
  (server-mode 1)
  :hook (server-switch-hook . 'raise-frame)
  :config
  (unless (server-running-p)
    (server-start)))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (use-package all-the-icons rainbow-delimiters aggressive-indent smartparens cider yaml-mode markdown-mode neotree magit company solarized-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
