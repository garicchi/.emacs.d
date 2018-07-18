(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-delimiter-face ((t (:inherit markdown-markup-face :foreground "#cc8fa3"))))
 '(markdown-list-face ((t (:inherit markdown-markup-face :foreground "#c28fcc"))))
 '(markdown-markup-face ((t (:inherit \#cc8fa3 :slant normal :weight normal))))
 '(markdown-pre-face ((t (:inherit (markdown-code-face default)))))
 '(mode-line-active ((t (:foreground "#f9f9f9" :background "#AD1457" :box nil :height 140))))
 '(powerline-active0 ((t (:foreground "#f9f9f9" :background "#424242" :box nil :height 130)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(package-selected-packages
   (quote
    (json-mode company-go flycheck-pos-tip helm anzu company flycheck go-mode markdown-mode monokai-theme multi-term neotree popup powerline smooth-scroll undo-tree web-mode all-the-icons use-package))))
