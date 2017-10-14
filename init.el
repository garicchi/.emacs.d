;; set japanese
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; set tab width
(setq-default tab-width 4)
(setq default-tab-width 4)

;; do not show startup message
(setq inhibit-startup-message t)

;; do not create backup
(setq make-backup-files nil)

;; remove auto save file when emacs finish
(setq delete-auto-save-files t)

;; show line code
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; show row num
(global-linum-mode t)

;; highlight cursor row
(global-hl-line-mode t)

;; bright corresponding brancket
(show-paren-mode 1)

;; yes or no convert to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; change option key to meta on mac
(setq mac-option-modifier 'meta)

;; enable package tool
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(fset 'package-desc-vers 'package--ac-desc-version)
(package-initialize)

;; Auto Complete
(unless (package-installed-p 'auto-complete)
  (package-refresh-contents) (package-install 'auto-complete))

(ac-config-default)
(add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ

;; Move Window By Arrow Key
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <down>")  'windmove-down)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(define-key global-map (kbd "C-c <left>") 'split-window-horizontally)
(define-key global-map (kbd "C-c <right>") 'split-window-horizontally)
(define-key global-map (kbd "C-c <up>") 'split-window-vertically)
(define-key global-map (kbd "C-c <down>") 'split-window-vertically)

;; Share Clipboard
(setq x-select-enable-clipboard t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("4020c1449293512850824de75244ade2d2abfc2284854a6e95205b4ae6c84035" default)))
 '(package-selected-packages
   (quote
	(atom-dark-theme neotree markdown-mode auto-complete))))

;; Markdown Mode
(setq x-select-enable-clipboard t)
(add-to-list 'auto-mode-alist'("\\.md\\'" . markdown-mode))

;; color theme
;; (load-theme 'wombat t)

;; hide menu bar
(menu-bar-mode -1)

;; enable neotree
(unless (package-installed-p 'neotree)
  (package-refresh-contents) (package-install 'neotree))

(setq neo-show-hidden-files t)
(neotree)


(unless (package-installed-p 'atom-one-dark-theme)
  (package-refresh-contents) (package-install 'atom-one-dark-theme))

(unless (package-installed-p 'flycheck)
  (package-refresh-contents) (package-install 'flycheck))


(load-theme 'atom-one-dark t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "color-235" :foreground "#ABB2BF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(linum ((t (:background "color-235" :foreground "#666D7A")))))
