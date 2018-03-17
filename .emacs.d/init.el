(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(defun modern-emacs-version ()
  (or (>= emacs-major-version 25)
      (and (= emacs-major-version 24)
           (>= emacs-minor-version 4))))

(defun require-no-error (package)
  (require package nil :no-error))

(when (modern-emacs-version)
  (toggle-frame-maximized))
(split-window-right)
(setq inhibit-startup-screen t)
(setq column-number-mode t)
(setq make-backup-files nil) ; turn off storing backup files (on save) under the original name with a ~ appended
(setq auto-save-default nil) ; turn off storing auto-save files intermittently with a file name on the form #file#
(setq-default indent-tabs-mode nil) ; turn off tabs e.g. in Markdown mode

(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'prog-mode-hook
	  (lambda()
	    (local-set-key (kbd "C-c <right>") 'hs-show-block)
	    (local-set-key (kbd "C-c <left>") 'hs-hide-block)
	    (local-set-key (kbd "C-c <up>") 'hs-hide-all)
	    (local-set-key (kbd "C-c <down>") 'hs-show-all)
	    (hs-minor-mode t)))

(eval-after-load "smartparens-mode"
  '(progn
     (add-hook 'prog-mode-hook #'smartparens-mode)
     (add-hook 'cider-repl-mode-hook #'smartparens-mode)))

(eval-after-load "rainbow-delimiters"
  '(progn
     (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
     (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)))

(eval-after-load "aggressive-indent-mode"
  '(progn
     (add-hook 'clojure-mode-hook #'aggressive-indent-mode) 
     (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)))

(when (require-no-error 'auto-complete)
  (global-auto-complete-mode t))

(when (require-no-error 'ac-cider)
  (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
  (add-hook 'cider-mode-hook 'ac-cider-setup)
  (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
  (eval-after-load "auto-complete"
    '(progn
       (add-to-list 'ac-modes 'cider-mode)
       (add-to-list 'ac-modes 'cider-repl-mode))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right. 
 '(package-selected-packages
   (quote
    (magit php-mode web-mode markdown-mode yaml-mode ac-cider cider aggressive-indent rainbow-delimiters smartparens auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
