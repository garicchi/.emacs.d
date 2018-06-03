;;
;;      共通設定
;;
(add-to-list 'load-path "elisp")
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
(setq auto-save-default nil)

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

(setq markdown-command "/usr/local/bin/multimarkdown")

;; はみ出した表示をウインドウの右端で折り返さない
;; (setq-default truncate-lines t)
;; (setq-default truncate-partial-width-windows t)

;; バッファが外部から変更されたときに自動で再読込
(global-auto-revert-mode 1)

;; MacのoptionキーをMetaキーに割り当てる
(setq mac-option-modifier 'meta)

;; 背景を透過する
(set-frame-parameter nil 'alpha 95 )

;; 起動時のフレーム設定
;;(setq initial-frame-alist
;;   (append (list
;;      '(top . 0)
;;      '(left . 0)
;;      '(width . 200)
;;      '(height . 60)
;;	  )
;;	  initial-frame-alist
;;	  )
;;   )
;;(setq default-frame-alist initial-frame-alist)

;; 起動時に分割
;;(setq w (selected-window))
;;(setq w2 (split-window w nil t))
;;(setq w3 (split-window w2 nil))

;;
;;     パッケージ設定
;;
;; emacsが自動的に生成する設定を別ファイルに
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; package.elを有効化
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(fset 'package-desc-vers 'package--ac-desc-version)
(package-initialize)

;; multi-term
(unless (package-installed-p 'multi-term)
  (package-refresh-contents) (package-install 'multi-term))
(setq multi-term-program shell-file-name)


;; auto-complete.el
(unless (package-installed-p 'auto-complete)
  (package-refresh-contents) (package-install 'auto-complete))

(ac-config-default)
;; 各モードでauto-completeを有効化する
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode) 
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(add-to-list 'ac-modes 'go-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ

;; all-the-icons
(unless (package-installed-p 'all-the-icons)
  (package-refresh-contents)
  (package-install 'all-the-icons)
  (all-the-icons-install-fonts)
  )

;; neotree
(unless (package-installed-p 'neotree)
  (package-refresh-contents) (package-install 'neotree))

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(setq neo-show-hidden-files t)
(neotree)

;; flycheck
(unless (package-installed-p 'flycheck)
  (package-refresh-contents) (package-install 'flycheck))

(add-hook 'after-init-hook #'global-flycheck-mode)

;; web-mode
(unless (package-installed-p 'web-mode)
  (package-refresh-contents) (package-install 'web-mode))

;; web-mode
(unless (package-installed-p 'web-mode)
  (package-refresh-contents) (package-install 'web-mode))

(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue?$"     . web-mode))

;; go-mode
(unless (package-installed-p 'go-mode)
  (package-refresh-contents) (package-install 'go-mode))

(with-eval-after-load 'go-mode
  ;; auto-complete
  (unless (package-installed-p 'go-autocomplete)
	(package-refresh-contents) (package-install 'go-autocomplete))
 )


;; powerline
(unless (package-installed-p 'powerline)
  (package-refresh-contents) (package-install 'powerline))

(custom-set-faces
 '(mode-line ((t (:foreground "#f9f9f9" :background "#AD1457" :box nil :height 140))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil :height 140))))
 '(powerline-active1 ((t (:foreground "#f9f9f9" :background "#666666" :box nil :height 140))))
 '(powerline-active2 ((t (:foreground "#f9f9f9" :background "#AD1457" :box nil :height 140))))
 '(powerline-active0 ((t (:foreground "#f9f9f9" :background "#880E4F" :box nil :height 140))))
 )

(powerline-default-theme)


;; anything
(unless (package-installed-p 'anything)
  (package-refresh-contents) (package-install 'anything))

(require 'anything)
(require 'anything-startup)
(require 'anything-config)

(setq anything-sources
      '(anything-c-source-buffers+
	anything-c-source-recentf
	anything-c-source-emacs-commands
	anything-c-source-emacs-functions
	anything-c-source-files-in-current-dir
	anything-c-source-colors
	anything-c-source-man-pages
	))

;; undo-tree
(unless (package-installed-p 'undo-tree)
  (package-refresh-contents) (package-install 'undo-tree))
(require 'undo-tree)
(global-undo-tree-mode t)

(unless (package-installed-p 'smooth-scroll)
  (package-refresh-contents) (package-install 'smooth-scroll))
(require 'smooth-scroll)
(smooth-scroll-mode t)


;;
;;     キーバインド設定
;;

;; ウインドウ移動
(global-set-key (kbd "C-<left>")  'windmove-left)
(global-set-key (kbd "C-<down>")  'windmove-down)
(global-set-key (kbd "C-<up>")    'windmove-up)
(global-set-key (kbd "C-<right>") 'windmove-right)

;; ウインドウ分割
(global-set-key (kbd "M-<left>") 'split-window-horizontally)
(global-set-key (kbd "M-<right>") 'split-window-horizontally)
(global-set-key (kbd "M-<up>") 'split-window-vertically)
(global-set-key (kbd "M-<down>") 'split-window-vertically)

;; コピー
(global-set-key (kbd "C-q") 'copy-region-as-kill)
;; (global-set-key (kbd "C-r") 'yank)

;; 置換
(global-set-key (kbd "C-e") 'query-replace)

;; anything
(global-set-key (kbd "M-a") 'anything)

(global-set-key (kbd "C-,") 'scroll-up)
(global-set-key (kbd "C-.") 'scroll-down)


;; バッファの末尾と最初に移動
(global-set-key (kbd "M-,") 'beginning-of-buffer)
(global-set-key (kbd "M-.") 'end-of-buffer)

;; カーソル移動コマンド
(global-set-key (kbd "C-l") 'forward-char)
(global-set-key (kbd "C-k") 'backward-char)

;; バッファリスト
(global-set-key (kbd "C-<tab>") 'buffer-menu)
;; (global-set-key (kbd "M-<tab>") 'switch-to-buffer)

;; multi-term
(global-set-key (kbd "C-t") 'multi-term)

;; バッファリストを別ウインドウで開かないようにする
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; ウインドウをリサイズできるようにする
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))

(global-set-key (kbd "C-x r") 'window-resizer)


;;
;;     カラーテーマ設定
;;

;;(unless (package-installed-p 'atom-one-dark-theme)
;;  (package-refresh-contents) (package-install 'atom-one-dark-theme))
;;
;;(load-theme 'atom-one-dark t)
;;(custom-set-faces
;;

(unless (package-installed-p 'monokai-theme)                  
  (package-refresh-contents) (package-install 'monokai-theme))

(load-theme 'monokai t)
