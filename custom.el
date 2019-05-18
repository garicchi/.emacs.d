(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-buffer-file-name-style (quote truncate-with-project) t)
 '(doom-modeline-icon t t)
 '(doom-modeline-minor-modes nil t)
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-ls-git helm-source-files-in-current-dir helm-source-emacs-commands-history helm-source-emacs-commands helm-source-ls-git))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "unspecified-bg" :foreground "unspecified-fg" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(all-the-icons-blue ((t (:foreground "blue"))))
 '(doom-neotree-dir-face ((t (:foreground "blue"))))
 '(doom-neotree-file-face ((t (:foreground "unspecified-fg"))))
 '(doom-neotree-text-file-face ((t (:foreground "unspecified-fg"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "brightblack"))))
 '(font-lock-function-name-face ((t (:foreground "magenta"))))
 '(font-lock-keyword-face ((t (:foreground "blue"))))
 '(font-lock-negation-char-face ((t (:inherit bold :foreground "blue"))))
 '(font-lock-preprocessor-face ((t (:inherit bold :foreground "blue"))))
 '(font-lock-string-face ((t (:foreground "green"))))
 '(font-lock-type-face ((t (:foreground "yellow"))))
 '(helm-ff-file ((t (:foreground "unspecified-fg"))))
 '(helm-ff-prefix ((t (:foreground "red"))))
 '(helm-grep-match ((t (:foreground "red"))))
 '(helm-match ((t (:inherit bold :foreground "blue"))))
 '(helm-selection ((t (:inherit bold :background "red"))))
 '(helm-source-header ((t (:background "magenta" :foreground "#525252"))))
 '(highlight ((t (:background "red" :foreground "black"))))
 '(lazy-highlight ((t (:background "red" :foreground "white" :weight bold))))
 '(line-number ((t (:inherit default :foreground "brightblack" :strike-through nil :underline nil :slant normal :weight normal))))
 '(linum ((t (:inherit default :foreground "brightblack" :strike-through nil :underline nil :slant normal :weight normal))))
 '(markdown-code-face ((t (:background "unspecified-bg"))))
 '(markdown-header-face ((t (:inherit bold :foreground "blue"))))
 '(mode-line ((t (:background "unspecified-bg" :box nil))))
 '(neo-dir-link-face ((t (:foreground "blue"))))
 '(neo-expand-btn-face ((t (:foreground "blue"))))
 '(neo-root-dir-face ((t (:foreground "green" :box 4))))
 '(neo-vc-added-face ((t (:foreground "green"))))
 '(neo-vc-default-face ((t (:foreground "white"))))
 '(neo-vc-ignored-face ((t (:foreground "brightblack"))))
 '(region ((t (:background "red"))))
 '(swiper-isearch-current-match ((t (:background "red" :foreground "white"))))
 '(swiper-line-face ((t (:background "red" :foreground "black"))))
 '(vertical-border ((t (:background "unspecified-bg" :foreground "brightblack"))))
 '(whitespace-tab ((t (:background "unspecified-bg" :foreground "#4c566a" :underline nil)))))
