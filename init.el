;;
;;   common setting
;;

;; 独自のelispを入れるパスを設定
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; 日本語設定
(set-language-environment "Japanese")

;; UTF-8設定
(prefer-coding-system 'utf-8)

;; タブサイズ設定
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)

(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120 124 128))
(global-set-key (kbd "<backtab>")  'backtab-to-tab-stop)
;; backtab
(defun backtab-to-tab-stop ()
  "Do back to previous tab-stop"
  (interactive)
  (let ((tabs tab-stop-list)
        (col (current-column))
        (tab-last 0))
    (back-to-indentation)           ; 現在行の白文字でない最初の文字へポイントを移動
    (if (= col (current-column))        ; 当初のカーソル位置が白文字でない最初の文字位置と一致しているかどうかで期待する位置を調整
        (while (and tabs (> col (car tabs)))
          (setq tab-last (car tabs))
          (setq tabs (cdr tabs)))
      (while (and tabs (>= col (car tabs)))
        (setq tab-last (car tabs))
        (setq tabs (cdr tabs))))
                                        ; 期待するタブ位置にくるまで１文字ずつ削除
    (while (> (current-column) tab-last)
      (delete-backward-char 1))))

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

;; ロックファイルを作成しない
(setq create-lockfiles nil)

;; エラー音をならなくする
(setq ring-bell-function 'ignore)

;; 改行コードを表示する
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; scroll-barを消す
(scroll-bar-mode -1)

;; recentfの履歴を10000にする
(setq recentf-max-saved-items 1000)

;;
(setq recentf-exclude '(".recentf"))

(setq recentf-auto-cleanup 10)
(setq recentf-auto-save-timer
      (run-with-idle-timer 30 t 'recentf-save-list)
      )

;; 行番号を表示する
(global-linum-mode t)

;; 現在の行をハイライトする
(global-hl-line-mode 0)

;; 対応するカッコをハイライトする
(show-paren-mode 1)

;; yes or no convert to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; クリップボードをシステムと共有する
(setq x-select-enable-clipboard t)

(setq mac-option-key-is-meta t)

;; system-type predicates
;; from http://d.hatena.ne.jp/tomoya/20090807/1249601308
(setq darwin-p   (eq system-type 'darwin)
      linux-p    (eq system-type 'gnu/linux)
      carbon-p   (eq system-type 'mac)
      meadow-p   (featurep 'meadow))

; Emacs と Mac のクリップボード共有
; from http://hakurei-shain.blogspot.com/2010/05/mac.html
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(if (or darwin-p carbon-p)
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;; はみ出した表示をウインドウの右端で折り返さない
(setq-default truncate-lines nil)
(setq-default truncate-partial-width-windows nil)

;; term-modeで折り返さない
(add-hook 'term-mode-hook
          (lambda () (setq truncate-lines t)))

;; バッファが外部から変更されたときに自動で再読込
(global-auto-revert-mode 1)

;; MacのoptionキーをMetaキーに割り当てる
(setq mac-option-modifier 'meta)

;; 背景を透過する
;;(set-frame-parameter nil 'alpha 95 )

;; emacsが自動的に生成する設定を別ファイルに
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
;; ファイル名保管で大文字小文字の違いを無視する
(setq completion-ignore-case t)

;; 起動時のフレーム設定
;;(setq initial-frame-alist
;;   (append (list
;;      '(top . 0)
;;      '(left . 0)
;;      '(width . 800)
;;      '(height . 600)
;;)
;;initial-frame-alist
;;)
;;   )
;;(setq default-frame-alist initial-frame-alist)

;; 起動時に分割
;;(setq w (selected-window))
;;(setq w2 (split-window w nil t))
;;(setq w3 (split-window w2 nil))

;; リージョン内を置換するように
(setq transient-mark-mode t)

;; エラー音をならなくする
(setq ring-bell-function 'ignore)

;;; モードラインに時間を表示する
(display-time)

;; eww
(setq eww-search-prefix "https://www.google.co.jp/search?btnI&q=")

;; ewwを複数起動
(defun eww-mode-hook--rename-buffer ()
  "Rename eww browser's buffer so sites open in new page."
  (rename-buffer "eww" t))
(add-hook 'eww-mode-hook 'eww-mode-hook--rename-buffer)

;; ewwの背景色をなんとかする
(defvar eww-disable-colorize t)
(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg)))
(advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
(advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
(defun eww-disable-color ()
  "eww で文字色を反映させない"
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload))
(defun eww-enable-color ()
  "eww で文字色を反映させる"
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload))

;; window-resizerコマンド - ウインドウをリサイズできるようにする
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

;; window-toggle-max ウインドウを最大化する
(defvar is-window-maximized nil)

(defun window-toggle-max ()
  (interactive)
  (progn
    (if is-window-maximized
        (balance-windows)
      (maximize-window))
    (setq is-window-maximized (not is-window-maximized))))

;; rename機能
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; terminal終了時の確認を消す
(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

(add-hook 'term-exec-hook 'set-no-process-query-on-exit)

;;   ### diffを見やすく ###
;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)

(defun config-sh-mode ()
  (setq indent-tabs-mode nil 
        c-basic-offset 4) 
)
(add-hook 'sh-mode-hook 'config-sh-mode)

;;
;; package setting
;; http://emacs-jp.github.io/packages/package-management/package-el.html
;;

;; package.elを有効化
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(fset 'package-desc-vers 'package--ac-desc-version)
(package-initialize)

;; use-package パッケージマネージャ
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; neotreeのいい感じのアイコン
(use-package all-the-icons
  :ensure t)

;; undoをツリー表示してくれるやつ
(use-package undo-tree
  :ensure t
  :config
  (defun undo-tree-split-side-by-side (original-function &rest args)
  "Split undo-tree side-by-side"
  (let ((split-height-threshold nil)
        (split-width-threshold 0))
    (apply original-function args)))
  (advice-add 'undo-tree-visualize :around #'undo-tree-split-side-by-side)
  ;; visualizerはRETかC-gで終了
  (define-key undo-tree-visualizer-mode-map (kbd "RET") 'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-quit)
  
  :init
  (global-undo-tree-mode)
  )

;; カーソルを分身させるやつ
(use-package multiple-cursors
  :ensure t
  :bind* ("C-c C-m" . mc/mark-all-like-this)
  )

(use-package nyan-mode
  :ensure t
  :init
  (nyan-mode)
  (nyan-start-animation)
  )

(use-package wttrin
  :ensure t
  :config
  (setq wttrin-default-cities '("Tokyo"))
  (setq wttrin-default-accept-language '("Accept-Language" . "en-US"))
  )

(use-package fireplace
  :ensure t
  :config
  )

(use-package selectric-mode
  :ensure t
  :config
  )


;; 正規表現検索をビジュアル的に
(use-package visual-regexp
  :ensure t
  )

;; C-vとかのスクロールがスムーズになる
(use-package smooth-scroll
  :ensure t
  :config
  (smooth-scroll-mode t)
  (setq smooth-scroll/vscroll-step-size 8)
  )

;; モードラインがきれいになる
;;(use-package powerline
;;  :ensure t
;;  :config
;;  (defun shorten-directory (dir max-length)
;;    "Show up to `max-length' characters of a directory name `dir'."
;;    (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
;;          (output ""))
;;      (when (and path (equal "" (car path)))
;;        (setq path (cdr path)))
;;      (while (and path (< (length output) (- max-length 4)))
;;        (setq output (concat (car path) "/" output))
;;        (setq path (cdr path)))
;;      (when path
;;        (setq output (concat ".../" output)))
;;      output))
;;  
;;  (defun powerline-my-theme ()
;;    "Setup the my mode-line."
;;    (interactive)
;;    (setq-default header-line-format
;;                  '("%e"
;;                    (:eval
;;                     (let* ((active (powerline-selected-window-active))
;;                            (mode-line (if active 'mode-line 'mode-line-inactive))
;;                            (face1 (if active 'powerline-active1 'powerline-inactive1))
;;                            (face2 (if active 'powerline-active2 'powerline-inactive2))
;;                            (separator-left (intern (format "powerline-%s-%s"
;;                                                            powerline-default-separator
;;                                                            (car powerline-default-separator-dir))))
;;                            (separator-right (intern (format "powerline-%s-%s"
;;                                                             powerline-default-separator
;;                                                             (cdr powerline-default-separator-dir))))
;;                            (lhs (list (powerline-raw "%*" nil 'l)
;;;;                                       (powerline-buffer-size nil 'l)
;;;;                                       (powerline-raw mode-line-mule-info nil 'l)
;;                                       (powerline-raw
;;                                        (shorten-directory default-directory 15)
;;                                        nil 'l)
;;                                       (powerline-buffer-id nil 'r)
;;                                       (when (and (boundp 'which-func-mode) which-func-mode)
;;                                         (powerline-raw which-func-format nil 'l))
;;                                       (powerline-raw " ")
;;                                       (funcall separator-left mode-line face1)
;;                                       (when (boundp 'erc-modified-channels-object)
;;                                         (powerline-raw erc-modified-channels-object face1 'l))
;;                                       (powerline-vc face1 'r)
;;                                       (powerline-major-mode face1 'l)
;;                                       (powerline-process face1)
;;                                        ;(powerline-minor-modes face1 'l)
;;                                       (powerline-narrow face1 'l)
;;                                       (powerline-raw " " face1)
;;                                       (funcall separator-left face1 face2)
;;                                       ))
;;                            (rhs (list (powerline-raw global-mode-string face2 'r)
;;                                       (funcall separator-right face2 face1)
;;                                       (powerline-raw "%4l" face1 'l)
;;                                       (powerline-raw ":" face1 'l)
;;                                       (powerline-raw "%3c" face1 'r)
;;                                       (funcall separator-right face1 mode-line)
;;                                       (powerline-raw " ")
;;                                       (powerline-raw "%6p" nil 'r)
;;                                       (powerline-hud face2 face1))))
;;                       (concat (powerline-render lhs)
;;                               (powerline-fill face2 (powerline-width rhs))
;;                               (powerline-render rhs)))))))
;;  (powerline-my-theme)
;;  (setq-default mode-line-format nil)
;;  ;; powerlineの色を変えたい時はここ
;;  ;;(defun make/set-face (face-name fg-color bg-color weight)
;;  ;;  (make-face face-name)
;;  ;;  (set-face-attribute face-name nil
;;  ;;                      :foreground fg-color :background bg-color :box nil :weight weight))
;;  ;;(make/set-face 'mode-line-1-fg "#282C34" "#9b7cb6" 'bold)
;;  ;;(make/set-face 'mode-line-2-fg "#AAAAAA" "#2F343D" 'bold)
;;  ;;(make/set-face 'mode-line-1-arrow  "#AAAAAA" "#9b7cb6" 'bold)
;;  ;;(make/set-face 'mode-line-2-arrow  "#AAAAAA" "#3E4451" 'bold)
;;  )

;; doomなモードライン
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-minor-modes nil)
  :hook (after-init . doom-modeline-mode)
      )

;; flycheckとかでポップアップしてくれる
(use-package popup
  :ensure t)

;; 非アクティブウインドウが暗くなる
;;(use-package dimmer
;;  :ensure t
;;  :init (dimmer-mode)
;;  )


;; 左側にでるファイラー
(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-show-hidden-files t)
  (setq neo-smart-open t) ;ウインドウを開くたびにcurrent fileのあるディレクトリを表示
  
  (bind-key "C-c c" 'neotree-create-node neotree-mode-map)
  (bind-key "C-c d" 'neotree-delete-node neotree-mode-map)
  (bind-key "C-c r" 'neotree-rename-node neotree-mode-map)
  (bind-key "C-c p" 'neotree-copy-node neotree-mode-map)
  (bind-key "C-c s" 'neotree-stretch-toggle neotree-mode-map)
  (bind-key "C-c t" 'neotree-toggle neotree-mode-map)
  (bind-key "C-c n" 'neotree-create-node neotree-mode-map)
  (bind-key* "C-x n" 'neotree-refresh) ;バッファで開いているところをneoteeのルートにする
  
  (neotree)
  )

;; 複数開けるターミナル
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program shell-file-name)
  (bind-key "C-x t" 'multi-term)
  )

(use-package imenu-list
  :ensure t
  )


;;
;; color theme
;;

;; spacemacsのテーマ。とてもよい
;;(use-package spacemacs-theme
;;  :defer t
;;  :init (load-theme 'spacemacs-dark t)
;;  )

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-one t)
  :custom
  (doom-themes-enable-bold t)    ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled

  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )


;;(use-package monokai-theme
;;  :ensure t
;;  :config
;;  (load-theme 'monokai t)
;;  )
;;(use-package darcula-theme
;;  :ensure t
;;  :init (load-theme 'darcula t)
;;  )
;;(use-package color-theme-sanityinc-tomorrow
;;  :ensure t
;;  :init (load-theme 'sanityinc-tomorrow-eighties)
;;)
;;(use-package nord-theme
;;  :ensure t
;;  :init (load-theme 'nord t)
;;  )
;;(use-package zenburn-theme
;;  :ensure t
;;  :init (load-theme 'zenburn t)
;;  )

(use-package which-key
  :ensure t
  :init (which-key-mode)
  )

;; バッファをタブみたいに表示してくれるやつ
(use-package tabbar
  :ensure t
  :config
  (tabbar-mwheel-mode nil)                  ;; マウスホイール無効
  (setq tabbar-buffer-groups-function nil)  ;; グループ無効
  (setq tabbar-use-images nil)              ;; 画像を使わない
  ;; 左側のボタンを消す
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
                   (cons "" nil))))


  ;; タブのセパレーターの長さ
  (setq tabbar-separator '(1.0))
  ;; 表示するバッファ
  (defun my-tabbar-buffer-list ()
    (delq nil
          (mapcar #'(lambda (b)
                      (cond
                       ;; Always include the current buffer.
                       ((eq (current-buffer) b) b)
                       ((buffer-file-name b) b)
                       ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                       ((equal "*scratch*" (buffer-name b)) b) ; *scratch*バッファは表示する
                       ((char-equal ?* (aref (buffer-name b) 0)) nil) ; それ以外の * で始まるバッファは表示しない
                       ((buffer-live-p b) b)))
                  (buffer-list))))
                                        ;(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)
  :init (tabbar-mode 0)
  )

;; markdownをリアルタイムにプレビューしてくれる
(use-package markdown-preview-mode
  :ensure t
  :config
  (setq markdown-preview-stylesheets (list "github.css"))
  (setq markdown-command "multimarkdown")
  (bind-key "C-c p" 'markdown-preview-mode markdown-mode-map)
  )

;; 構文チェックとかしてくれる
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode) ; flycheck-pos-tip
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  )

;; コード補完とかしてくれる
(use-package company
  :ensure t
  :init (global-company-mode) ; 全バッファで有効にする
  :config
  (setq company-idle-delay 0) ; デフォルトは0.5
  (setq company-minimum-prefix-length 2) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  (add-to-list 'company-backends 'company-edbi)
  (global-set-key (kbd "C-<tab>") 'company-complete)
  )

;; companyにアイコンとか表示してくれるっぽい
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; コードブロックごとにregionを作れる
(use-package expand-region
  :ensure t
  :bind* ("C-c SPC" . er/expand-region)
  )

;; 検索時に一致した数をモードラインに出してくれる
(use-package anzu
  :ensure t
  :init (global-anzu-mode +1)
  )

;; 単語にカーソルを置くと同じ単語をハイライトしてくれる
(use-package highlight-symbol
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (setq highlight-symbol-idle-delay 1.0)
  )

;; 全てをインクリメンタルサーチ
(use-package helm
  :ensure t
  :init
  (helm-mode t)
  :config
  ;; emacsのコマンドを検索可能に
  (defvar helm-source-emacs-commands
    (helm-build-sync-source "Emacs commands"
      :candidates (lambda ()
                    (let ((cmds))
                      (mapatoms
                       (lambda (elt) (when (commandp elt) (push elt cmds))))
                      cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "A simple helm source for Emacs commands.")

  ;; emacsのコマンド履歴を検索可能に
  (defvar helm-source-emacs-commands-history
    (helm-build-sync-source "Emacs commands history"
      :candidates (lambda ()
                    (let ((cmds))
                      (dolist (elem extended-command-history)
                        (push (intern elem) cmds))
                      cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "Emacs commands history")

  ;; helm-mini時のソース一覧
  (custom-set-variables
   '(helm-mini-default-sources '(helm-source-buffers-list
                                 helm-source-recentf
                                 helm-source-ls-git
                                 helm-source-files-in-current-dir
                                 helm-source-emacs-commands-history
                                 helm-source-emacs-commands
                                 helm-source-ls-git
                                 )))
  ;; 曖昧マッチ
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        )

  ;; 下にウインドウとしてhelmを表示する
  (setq helm-default-display-buffer-functions '(display-buffer-in-atom-window))
  
  (bind-key* "M-x" 'helm-M-x)
  (bind-key* "C-x x" 'helm-M-x)
  (bind-key* "C-x RET" 'helm-mini)
  (bind-key* "C-x f" 'helm-find-files)
  (bind-key* "C-x b" 'helm-buffers-list)
  (bind-key* "C-x a" 'helm-do-grep-ag)
  )

;; helm-M-xでキーバインドを表示してくれる
(use-package helm-descbinds
  :ensure t
  :init (helm-descbinds-mode)
  )

;; helmで高速ファイル中身サーチ(ag)
(use-package helm-ag
  :ensure t
  )

;; helmでgit grep
(use-package helm-git-grep
  :ensure t
  :config
  :bind* ("C-x g" . helm-browse-project)
  :bind* ("C-x s" . helm-git-grep)
  )

;; helmでgit ls
(use-package helm-ls-git
  :ensure t
  )

;; flycheckでポップアップウインドウを出してくれる
(use-package flycheck-pos-tip
  :ensure t)

;; git diffを表示するやつ
(use-package git-gutter+
  :ensure t
  :config
  :init
  (global-git-gutter+-mode)
  )

(use-package git-gutter-fringe+
  :ensure t
  )

;; companyのgo拡張
(use-package company-go
  :ensure t
  :config
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))
  )

;; go-mode
(use-package go-mode
  :ensure t)

;; web-mode
(use-package web-mode
  :ensure t)

;; ruby-mode
(use-package ruby-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\Vagrantfile$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  )

;; json-mode
(use-package json-mode
  :ensure t)

;; php-mode
(use-package php-mode
  :ensure t)

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  )

;; glsl-mode
(use-package glsl-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.shader$" . glsl-mode))
  )

;; companyのglsl拡張
(use-package company-glsl
  :ensure t
  )

;; terraform0mode
(use-package terraform-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tf$" . terraform-mode))
  )

;; dockerfile-mode
(use-package dockerfile-mode
  :ensure t)

;; nginx-mode
(use-package nginx-mode
  :ensure t)

;; groovy-mode
(use-package groovy-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\Jenkinsfile$" . groovy-mode))
  (defun my-c-mode-hook () 
    (setq indent-tabs-mode nil 
          c-basic-offset 4)) 
  (add-hook 'c-mode-common-hook 'my-c-mode-hook) 

  )

;; prolog
(use-package prolog
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
  )

;; git操作
(use-package magit
  :ensure t
  :bind* ("C-x m" . magit-status)
  )

;; 空白を可視化
(use-package whitespace
  :config
  ;; 空白を表示
  (setq whitespace-style '(face           ; faceで可視化
                                        ;trailing       ; 行末
                           tabs           ; タブ
                           spaces         ; スペース
                           empty          ; 先頭/末尾の空行
                                        ;space-mark     ; 表示のマッピング
                           tab-mark
                           ))
  (defvar my/bg-color "#232323")
  (set-face-attribute 'whitespace-trailing nil
                                        ;:background my/bg-color
                      :foreground "DeepPink"
                      :underline t)
  (set-face-attribute 'whitespace-tab nil
                                        ;:background my/bg-color
                      :foreground "#4c566a"
                      :underline nil)
  (set-face-attribute 'whitespace-space nil
                                        ;:background my/bg-color
                      :foreground "GreenYellow"
                      :weight 'bold)
  (set-face-attribute 'whitespace-empty nil
                                        ;:background my/bg-color
                      )
  :init (global-whitespace-mode t)
  )

;; PATHをシェルから引き継ぐ
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize)
  )

;; companyのjedi拡張
(use-package company-jedi
  :ensure t
  )

;; フォースを信じろ
(use-package jedi
  :ensure t
  :config
  (setq jedi:complete-on-dot t)
  (bind-key "C-x d" 'jedi:goto-definition jedi-mode-map)
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  )

;; それっぽい定義にジャンプしてくれる
(use-package dumb-jump
  :config
  ;;(setq dumb-jump-selector 'ivy)
  (setq dumb-jump-selector 'helm)
  (bind-key "C-x d" 'dumb-jump-go)
  :ensure t
  )


;; インクリメンタルバッファサーチ
(use-package swiper
  :ensure t
  :config
  (bind-key* "C-s" 'swiper)
  )

;; plantuml-mode
(use-package plantuml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (setq plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.12/libexec/plantuml.jar")
  (setq plantuml-java-options "")
  (setq plantuml-output-type "png")
  (setq plantuml-options "-charset UTF-8")
  )

;; 分割ウインドウをいい感じの比率で制御してくれる
;;(use-package golden-ratio
;;  :ensure t
;;  :init (golden-ratio-mode t)
;;  :config
;;  (add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
;;  )

;; 起動画面
;;(use-package dashboard
;;  :ensure t
;;  :config
;;  (dashboard-setup-startup-hook)
;;  )

;;
;;  setting for key bind
;;

;; ウインドウ分割
(bind-key* "C-d" 'other-window)
(bind-key* "C-f" 'other-window)

(bind-key* "C-x l" 'windmove-left)
(bind-key* "C-x :" 'windmove-right)
(bind-key* "C-x p" 'windmove-up)
(bind-key* "C-x ;" 'windmove-down)

(bind-key* "C-x p" 'previous-buffer)

(bind-key* "C-x SPC" 'rectangle-mark-mode)

;; 置換コマンド
(bind-key* "C-r" 'vr/replace)

;; スキップ移動
(bind-key* "M-<down>" (kbd "C-u 5 <down>"))
(bind-key* "M-<up>" (kbd "C-u 5 <up>"))
(bind-key* "M-<right>" 'forward-word)
(bind-key* "M-<left>" 'backward-word)
;; たーみなる
(bind-key* "ESC <down>" (kbd "C-u 5 <down>"))
(bind-key* "ESC <up>" (kbd "C-u 5 <up>"))

;; C-backspaceで単語削除
(bind-key* "C-<backspace>" 'backward-kill-word)

;; コピー
(bind-key* "C-q" 'copy-region-as-kill)

(bind-key* "C-x C-b" 'buffer-menu)

;; eww
(bind-key* "C-x w" 'eww)

;; window resize
(bind-key* "C-x r" 'window-resizer)

;;
;; ファイルホック
;;

;; sql-mode
(add-to-list 'auto-mode-alist '("\\.sql?$"     . sql-mode))

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue?$"     . web-mode))

(defun pr-show ()
  "Open PullRequest"
  (interactive)
  (shell-command (concat "source ~/.order66/env.sh&&pr " (buffer-file-name) " " (number-to-string (line-number-at-pos))))
  )

(bind-key* "C-x p" 'pr-show)

(defun run-mode ()
  "chdmo u+x"
  (interactive)
  (shell-command (concat "chmod u+x " (buffer-file-name)))
  )

(defun commit-push ()
  "git commit git push"
  (interactive)
  (shell-command "git commit -a -m \"fix:\"&&git push")
  )

