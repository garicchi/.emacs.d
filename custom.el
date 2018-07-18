(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-files-in-current-dir helm-source-emacs-commands-history helm-source-emacs-commands)))
 '(package-selected-packages
   (quote
    (php-completion php-mode web-mode use-package undo-tree smooth-scroll powerline neotree multi-term monokai-theme markdown-mode json-mode helm flycheck-pos-tip company-go anzu all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-builtin-face ((t (:foreground "red" :weight normal))))
 '(font-lock-constant-face ((t (:foreground "magenta"))))
 '(font-lock-function-name-face ((t (:foreground "blue"))))
 '(font-lock-keyword-face ((t (:foreground "red" :weight normal))))
 '(font-lock-string-face ((t (:foreground "yellow"))))
 '(font-lock-type-face ((t (:foreground "green" :slant normal))))
 '(mode-line ((t (:foreground "#f9f9f9" :background "#AD1457" :box nil :height 140))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil :height 140))))
 '(neo-dir-link-face ((t (:background "#1B1E1C" :foreground "cyan"))))
 '(neo-root-dir-face ((t (:background "#1B1E1C" :foreground "green"))))
 '(powerline-active0 ((t (:foreground "#f9f9f9" :background "#880E4F" :box nil :height 140))))
 '(powerline-active1 ((t (:foreground "#f9f9f9" :background "#666666" :box nil :height 140))))
 '(powerline-active2 ((t (:foreground "#f9f9f9" :background "#AD1457" :box nil :height 140))))
 '(region ((t (:inherit highlight :background "red")))))
