;; set japanese
(set-language-environment "Japanese")

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
(require 'auto-complete-config)
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

;; Share Clipboard
(setq x-select-enable-clipboard t)
(custom-set-variables)

;; Markdown Mode
(setq x-select-enable-clipboard t)
(add-to-list 'auto-mode-alist'("\\.md\\'" . markdown-mode))

;; color theme
(load-theme 'wombat t)

;; enable neotree
(require 'neotree)
(neotree)


