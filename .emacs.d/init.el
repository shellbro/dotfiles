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
(setq tramp-default-method "ssh")

(toggle-frame-maximized)
(split-window-right)
(switch-to-buffer-other-window "*Messages*")

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(global-set-key (kbd "C-x C-S-f") (lambda ()
                                    (call-interactively 'find-file-other-window)
                                    (interactive)
                                    (other-window -1)))

(defun kill-buffer-right-window ()
  (interactive)
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (kill-this-buffer)
    (select-window win-curr)))
(global-set-key (kbd "C-x K") 'kill-buffer-right-window)

(setq require-final-newline t)

;; Do not use tabs for indentation (for example in Markdown mode)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

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

(use-package buffer-move
  :ensure t
  :bind (("C-S-<up>" . buf-move-up)
         ("C-S-<down>" . buf-move-down)
         ("C-S-<left>" . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(use-package cider
  :ensure t
  :config
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
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
                       (local-set-key (kbd "C-c <right>") 'hs-show-block)
                       (local-set-key (kbd "C-c <left>") 'hs-hide-block)
                       (local-set-key (kbd "C-c <up>") 'hs-hide-all)
                       (local-set-key (kbd "C-c <down>") 'hs-show-all)
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
