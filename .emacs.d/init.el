(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(setq column-number-mode t)
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)
(set-frame-font "Source Code Pro 11")
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)
(setq python-shell-interpreter "python3")
(setq tramp-default-method "ssh")

(toggle-frame-maximized)
(split-window-right)
(switch-to-buffer-other-window "*Messages*")

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(defun find-file-other-window-and-return ()
  (interactive)
  (let ((win-curr (selected-window)))
    (call-interactively 'find-file-other-window)
    (select-window win-curr)))
(global-set-key (kbd "C-x C-S-f") 'find-file-other-window-and-return)

(defun kill-buffer-other-window ()
  (interactive)
  (let ((win-curr (selected-window)))
    (switch-to-buffer-other-window "foo")
    (kill-this-buffer)
    (kill-this-buffer)
    (select-window win-curr)))
(global-set-key (kbd "C-x K") 'kill-buffer-other-window)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq sh-basic-offset 2)
(setq python-indent-offset 2)
(setq require-final-newline t)

;; Turn off storing auto-save files intermittently with a file name on the
;; form #file#
(setq auto-save-default nil)
;; Turn off storing backup files (on save) under the original name with
;; a ~ appended
(setq make-backup-files nil)

(require 'use-package)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(use-package winner
  :init
  (winner-mode))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(use-package cider
  :ensure t
  :config
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq cider-repl-display-help-banner nil)
  (setq cider-auto-select-error-buffer nil)
  (setq cider-save-file-on-load nil))

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
  :hook (prog-mode . (lambda()
                       (local-set-key (kbd "C-S-<right>") 'hs-show-block)
                       (local-set-key (kbd "C-S-<left>") 'hs-hide-block)
                       (local-set-key (kbd "C-S-<up>") 'hs-hide-all)
                       (local-set-key (kbd "C-S-<down>") 'hs-show-all)
                       (hs-minor-mode t))))

(use-package sgml-mode
  :init
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"

                 "<!--"
                 sgml-skip-tag-forward
                 nil))
  :hook (nxml-mode . (lambda()
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
  (setq neo-theme 'ascii)
  (setq neo-autorefresh nil))

(use-package server
  :init
  (server-mode 1)
  :hook (server-switch-hook . 'raise-frame)
  :config
  (unless (server-running-p)
    (server-start)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
