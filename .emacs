;;; package --- Summary
;;; Themes and code completion.
;;; Commentary:
;;; Make flycheck shut up
;;; Code:
(package-initialize)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(dolist (package '(use-package))
   (unless (package-installed-p package)
     (package-install package)))

;; Autosave directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Add path for languageserver
(setq exec-path (append exec-path '("/home/jan/.local/bin")))

;; Theme settings
(use-package gruber-darker-theme
  :ensure t)
(load-theme 'gruber-darker t)

(set-frame-font "Iosevka 16" nil t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
(global-display-line-numbers-mode)
(setq display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)

;; forgot what this does
(windmove-default-keybindings)
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

;; IDE setup
(use-package lsp-mode
  :ensure t)
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(use-package yasnippet
  :ensure t)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(setq flycheck-global-modes '(not ess-mode ess-r-mode ESS))

(use-package lsp-treemacs
  :ensure t)
(use-package projectile
  :ensure t)
(use-package company
  :ensure t)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-global-modes '(not eshell-mode))

(use-package multiple-cursors
  :ensure t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

(use-package avy
  :ensure t)
(global-set-key (kbd "C-.") 'avy-goto-char)

(use-package which-key
  :ensure t)
(which-key-mode)

(use-package dap-mode
  :ensure t)
(use-package magit
  :ensure t)

;; ESS setup
(use-package ess
  :ensure t)

(use-package cmake-mode
  :ensure t)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'cmake-mode-hook 'lsp)
(add-hook 'ess-mode-hook 'lsp)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (treemacs-resize-icons 22)
   )
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
         ("C-x t t"   . treemacs)
         ("C-x t C-t" . treemacs-find-file)
	 ))
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))


(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(multiple-cursors r-autoyas yasnippet xcscope which-key use-package treemacs-projectile treemacs-magit gruber-darker-theme flycheck ess dap-mode company cmake-font-lock)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
