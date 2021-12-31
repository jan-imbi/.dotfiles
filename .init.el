;;; package --- Summary
;;; My emacs config
;;; Commentary:
;;; These boilerplate lines make flycheck shut up
;;; Code:

;; MELPA and use-package initialization
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

;; Languageserver config
(setq exec-path (append exec-path '("/home/jan/.local/bin")))
(use-package lsp-mode
  :ensure t)

;; Display context menu help when typing key combinations and autocompletion for buffers
(use-package which-key
  :ensure t)
(which-key-mode)
(use-package ido
  :ensure t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Theme settings
;(use-package gruber-darker-theme
;  :ensure t)
;(load-theme 'gruber-darker t)
;  :ensure t)
(use-package zenburn-theme
  :ensure t)
(load-theme 'zenburn t)
(set-frame-font "Iosevka 20" nil t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
(global-display-line-numbers-mode)
(setq display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)

;; Open in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Window navigation and force vertical splitting
(windmove-default-keybindings)
(use-package transpose-frame
  :ensure t)
(global-set-key (kbd "C-x p") 'transpose-frame)
(winner-mode 1)
(global-set-key (kbd "C-+") 'other-window)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Better scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(use-package golden-ratio-scroll-screen
  :ensure t)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)

;; org mode and pdf-tools
(use-package org
  :ensure t)

;; Code completion and modes
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(use-package yasnippet
  :ensure t)
(use-package company
  :ensure t)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-global-modes '(not eshell-mode))
(setq company-dabbrev-ignore-buffers 'pdf-view-mode)
; (setq company-dabbrev-other-buffers nil)
(use-package ess
  :ensure t)
(use-package poly-markdown :ensure t)
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(use-package poly-R
  :ensure t)
(use-package cmake-mode
  :ensure t)

;; Multicursor and visible text navigation
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

;; Static code analysis and debugging
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(setq flycheck-global-modes '(not ess-mode ess-r-mode ESS))
(use-package dap-mode
  :ensure t)

;; LSP hooks
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'cmake-mode-hook 'lsp)
(add-hook 'ess-mode-hook 'lsp)

(provide '.init)
;;; .init ends here
