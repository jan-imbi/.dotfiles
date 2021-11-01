;;; package --- Summary
;;; My emacs config
;;; Commentary:
;;; These boilerplate lines make flycheck shut up
;;; Code:

(load "~/.dotfiles/.init.el")
(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5eed5311ae09ed84cb2e4bf2f033eb4df27e7846a68e4ea3ab8d28f6b017e44a" "c3542d6868bbdac1667fd1a7d751fd41520ca63f155b043ffbc4f9b9effb1783" default))
 '(package-selected-packages
   '(zenburn-theme espresso-theme tango-plus-theme tango-plus whiteboard-theme whiteboard leuven-theme Leuven-theme leuven winner-mode popwin poly-R poly-r poly-markdown yasnippet-snippets xcscope which-key use-package treemacs-projectile treemacs-magit transpose-frame org-ref org-noter-pdftools multiple-cursors gruber-darker-theme golden-ratio-scroll-screen flycheck ess dap-mode company cmake-font-lock auto-complete))
 '(popwin:special-display-config
   '(("*polymode export*" :position bottom :noselect t)
     ("*Miniedit Help*" :noselect t)
     (help-mode)
     (completion-list-mode :noselect t)
     (compilation-mode :noselect t)
     (grep-mode :noselect t)
     (occur-mode :noselect t)
     ("*Pp Macroexpand Output*" :noselect t)
     ("*Shell Command Output*")
     ("*vc-diff*")
     ("*vc-change-log*")
     (" *undo-tree*" :width 60 :position right)
     ("^\\*anything.*\\*$" :regexp t)
     ("*slime-apropos*")
     ("*slime-macroexpansion*")
     ("*slime-description*")
     ("*slime-compilation*" :noselect t)
     ("*slime-xref*")
     (sldb-mode :stick t)
     (slime-repl-mode)
     (slime-connection-list-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
