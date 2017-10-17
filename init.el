;;
;;      共通設定
;;

;; 日本語設定
(set-language-environment "Japanese")
;; UTF-8設定
(prefer-coding-system 'utf-8)

;; タブサイズ設定
(setq-default tab-width 4)
(setq default-tab-width 4)

;; メニューバー削除
(menu-bar-mode -1)

;; ツールバー削除
(tool-bar-mode 0)

;; スタートメッセージを表示させない
(setq inhibit-startup-message t)

;; バックアップファイルを作成しない
(setq make-backup-files nil)

;; 自動保存ファイルを削除する
(setq delete-auto-save-files t)

;; 改行コードを表示する
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; 行番号を表示する
(global-linum-mode t)

;; 現在の行をハイライトする
(global-hl-line-mode t)

;; 対応するカッコをハイライトする
(show-paren-mode 1)

;; yes or no convert to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; クリップボードをシステムと共有する
(setq x-select-enable-clipboard t)

;; Markdown Modeの有効化
(add-to-list 'auto-mode-alist'("\\.md\\'" . markdown-mode))

;; MacのoptionきーをMetaキーに割り当てる
(setq mac-option-modifier 'meta)

;;
;;     パッケージ設定
;;

;; package.elを有効化
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(fset 'package-desc-vers 'package--ac-desc-version)
(package-initialize)

;; auto-complete.el
(unless (package-installed-p 'auto-complete)
  (package-refresh-contents) (package-install 'auto-complete))

(ac-config-default)
;; 各モードでauto-completeを有効化する
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode) 
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ

;; neotree
(unless (package-installed-p 'neotree)
  (package-refresh-contents) (package-install 'neotree))

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(setq neo-show-hidden-files t)
(neotree)

;; flycheck
(unless (package-installed-p 'flycheck)
  (package-refresh-contents) (package-install 'flycheck))

;; web-mode
(unless (package-installed-p 'web-mode)
  (package-refresh-contents) (package-install 'web-mode))

(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue?$"     . web-mode))

;; powerline
(unless (package-installed-p 'powerline)
  (package-refresh-contents) (package-install 'powerline))
(powerline-default-theme)

;; all-the-icons
(unless (package-installed-p 'all-the-icons)
  (package-refresh-contents)
  (package-install 'all-the-icons)
  (all-the-icons-install-fonts)
  )



;; ---------------------------------------------------------------------------


;;
;;     キーバインド設定
;;

(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <down>")  'windmove-down)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(global-set-key (kbd "C-c <left>") 'split-window-horizontally)
(global-set-key (kbd "C-c <right>") 'split-window-horizontally)
(global-set-key (kbd "C-c <up>") 'split-window-vertically)
(global-set-key (kbd "C-c <down>") 'split-window-vertically)

(global-set-key (kbd "C-q") 'kill-ring-save)

;;
;;     カラーテーマ設定
;;

;;(unless (package-installed-p 'atom-one-dark-theme)
;;  (package-refresh-contents) (package-install 'atom-one-dark-theme))
;;
;;(load-theme 'atom-one-dark t)
;;(custom-set-faces
;;
;; '(default ((t (:inherit nil :stipple nil :background "color-235" :foreground "#ABB2BF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
;; '(linum ((t (:background "color-235" :foreground "#666D7A")))))

(unless (package-installed-p 'monokai-theme)                  
  (package-refresh-contents) (package-install 'monokai-theme))

(load-theme 'monokai t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(spaceline-all-the-icons web-mode powerline neotree monokai-theme markdown-mode flycheck auto-complete atom-one-dark-theme atom-dark-theme all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
