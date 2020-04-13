(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)
(setq column-number-mode t)
(set-frame-font "Source Code Pro 11")

;; Disable auto-save files (#filename#)
(setq auto-save-default nil)
;; Disable backup files on save (filename~)
(setq make-backup-files nil)

(setq require-final-newline t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(put 'narrow-to-region 'disabled nil)
(setq tramp-default-method "ssh")

(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)

(toggle-frame-maximized)
(split-window-right)
(switch-to-buffer-other-window "*Messages*")
(add-hook 'emacs-startup-hook '(lambda () (other-window 1)))

(defun find-file-other-window-and-return ()
  (interactive)
  (let ((win-curr (selected-window)))
    (call-interactively 'find-file-other-window)
    (select-window win-curr)))

(defun kill-buffer-other-window ()
  (interactive)
  (let ((win-curr (selected-window)))
    (switch-to-buffer-other-window "foo2707")
    (kill-this-buffer)
    (kill-this-buffer)
    (select-window win-curr)))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x C-S-f") 'find-file-other-window-and-return)
(global-set-key (kbd "C-x K") 'kill-buffer-other-window)

(require 'use-package)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(use-package winner
  :init
  (winner-mode))

(use-package whitespace
  :hook (after-init . global-whitespace-mode)
  :config
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind ("C-c c" . company-complete))

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

(use-package hideshow
  :hook (prog-mode . (lambda()
                       (local-set-key (kbd "C-S-<right>") 'hs-show-block)
                       (local-set-key (kbd "C-S-<left>") 'hs-hide-block)
                       (local-set-key (kbd "C-S-<up>") 'hs-hide-all)
                       (local-set-key (kbd "C-S-<down>") 'hs-show-all)
                       (hs-minor-mode t))))

(use-package smartparens
  :ensure t
  :hook ((prog-mode cider-repl-mode) . smartparens-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode cider-repl-mode) . rainbow-delimiters-mode))

(defun aggressive-indent-indent-buffer ()
  (interactive)
  (aggressive-indent-indent-region-and-on (point-min) (point-max)))

(use-package aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode clojure-mode) . aggressive-indent-mode)
  :bind ("C-c i" . aggressive-indent-indent-buffer))

(defun cider-connect-if-repl-running ()
  (let ((path (concat default-directory "../../.nrepl-port")))
    (when (and (string= (file-name-nondirectory (buffer-file-name)) "core.clj")
               (file-exists-p path))
      (let ((host "localhost")
            (port (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string))))
        (when (= 0 (call-process "nc" nil nil nil "-z" host port))
          (cider-connect `(:host ,host :port ,port)))))))

(use-package cider
  :ensure t
  :hook (clojure-mode . cider-connect-if-repl-running)
  :config
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq cider-repl-display-help-banner nil)
  (setq cider-save-file-on-load nil))

(use-package dockerfile-mode
  :ensure t)

(use-package sh-script
  :config
  (setq sh-basic-offset 2))

(use-package python
  :config
  (setq python-shell-interpreter "python3"))

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
                       (local-set-key (kbd "C-S-<right>") 'hs-show-block)
                       (local-set-key (kbd "C-S-<left>") 'hs-hide-block)
                       (local-set-key (kbd "C-S-<up>") 'hs-hide-all)
                       (local-set-key (kbd "C-S-<down>") 'hs-show-all)
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
  :hook (server-switch-hook . raise-frame)
  :config
  (unless (server-running-p)
    (server-start)))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 3)
  (auto-package-update-maybe))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
