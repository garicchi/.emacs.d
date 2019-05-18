(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-ls-git helm-source-files-in-current-dir helm-source-emacs-commands-history helm-source-emacs-commands helm-source-ls-git)))
 '(package-selected-packages
   (quote
    (spacemacs-theme yaml-mode wttrin which-key web-mode visual-regexp use-package undo-tree terraform-mode tabbar swiper smooth-scroll selectric-mode powerline plantuml-mode php-mode nyan-mode nginx-mode neotree multiple-cursors multi-term markdown-preview-mode magit json-mode jedi imenu-list highlight-symbol helm-ls-git helm-git-grep helm-descbinds helm-ag groovy-mode flycheck-pos-tip fireplace expand-region exec-path-from-shell dumb-jump doom-themes doom-modeline dockerfile-mode company-jedi company-go company-glsl company-box anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "unspecified-bg" :foreground "unspecified-fg" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(all-the-icons-blue ((t (:foreground "blue"))))
 '(doom-neotree-dir-face ((t (:foreground "magenta"))))
 '(doom-neotree-file-face ((t (:foreground "unspecified-fg"))))
 '(doom-neotree-text-file-face ((t (:foreground "unspecified-fg"))))
 '(error ((t (:foreground "red" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "cyan"))))
 '(font-lock-comment-face ((t (:foreground "brightblack"))))
 '(font-lock-constant-face ((t (:foreground "magenta"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "brightblack"))))
 '(font-lock-function-name-face ((t (:foreground "magenta"))))
 '(font-lock-keyword-face ((t (:foreground "blue"))))
 '(font-lock-negation-char-face ((t (:inherit bold :foreground "blue"))))
 '(font-lock-preprocessor-face ((t (:inherit bold :foreground "blue"))))
 '(font-lock-string-face ((t (:foreground "green"))))
 '(font-lock-type-face ((t (:foreground "yellow"))))
 '(font-lock-variable-name-face ((t (:foreground "magenta"))))
 '(helm-candidate-number ((t (:background "unspecified-bg" :foreground "black"))))
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
 '(mode-line-inactive ((t (:inherit mode-line :background "unspecified-bg" :foreground "grey20" :box (:line-width -1 :color "grey75") :weight light))))
 '(neo-dir-link-face ((t (:foreground "magenta"))))
 '(neo-expand-btn-face ((t (:foreground "magent"))))
 '(neo-root-dir-face ((t (:foreground "green" :box 4))))
 '(neo-vc-added-face ((t (:foreground "green"))))
 '(neo-vc-default-face ((t (:foreground "white"))))
 '(neo-vc-ignored-face ((t (:foreground "brightblack"))))
 '(powerline-active1 ((t (:background "magenta" :foreground "black"))))
 '(powerline-active2 ((t (:inherit mode-line :background "magenta" :foreground "#dfdfdf"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "black"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "black"))))
 '(region ((t (:background "red"))))
 '(swiper-isearch-current-match ((t (:background "red" :foreground "white"))))
 '(swiper-line-face ((t (:background "red" :foreground "black"))))
 '(vertical-border ((t (:background "unspecified-bg" :foreground "brightblack"))))
 '(whitespace-hspace ((t (:background "unspecified-bg" :foreground "lightgray"))))
 '(whitespace-space ((t (:background "unspecified-bg" :foreground "GreenYellow" :weight bold))))
 '(whitespace-tab ((t (:background "unspecified-bg" :foreground "#4c566a" :underline nil)))))
