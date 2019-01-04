;;   ####  共通設定  ####

;; elispを入れるパスを指定

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

;; はみ出した表示をウインドウの右端で折り返さない
(setq-default truncate-lines nil)
(setq-default truncate-partial-width-windows nil)

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

                                        ;(ido-mode 1)
                                        ;(ido-everywhere 1)

                                        ;(setq ido-enable-flex-matching t) ;; 中間/あいまい一致

;; リージョン内を置換するように
(setq transient-mark-mode t)

;; エラー音をならなくする
(setq ring-bell-function 'ignore)

;; eww
(setq eww-search-prefix "https://www.google.co.jp/search?btnI&q=")

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

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; terminal終了時の確認を消す
(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

(add-hook 'term-exec-hook 'set-no-process-query-on-exit)

;;  ###  パッケージ設定  ###
;; http://emacs-jp.github.io/packages/package-management/package-el.html

;; ###  パッケージ設定 ###

;; package.elを有効化
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(fset 'package-desc-vers 'package--ac-desc-version)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package all-the-icons
  :ensure t)
(use-package web-mode
  :ensure t)
(use-package ruby-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\Vagrantfile$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  )
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  )

(use-package smooth-scroll
  :ensure t
  :config
  (smooth-scroll-mode t)
  (setq smooth-scroll/vscroll-step-size 8)

  )
(use-package powerline
  :ensure t
  :config
  (defun powerline-my-theme ()
    "Setup the my mode-line."
    (interactive)
    (setq powerline-current-separator 'utf-8)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face1 (if active 'mode-line-1-fg 'mode-line-2-fg))
                            (face2 (if active 'mode-line-1-arrow 'mode-line-2-arrow))
                            (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))
                            (lhs (list (powerline-raw " " face1)
                                       (powerline-major-mode face1)
                                       (powerline-raw " " face1)
                                       (funcall separator-left face1 face2)
                                       (powerline-buffer-id nil )
                                       (powerline-raw " [ ")
                                       (powerline-raw mode-line-mule-info nil)
                                       (powerline-raw "%*")
                                       (powerline-raw " |")
                                       (powerline-process nil)
                                       (powerline-vc)
                                       (powerline-raw " ]")
                                       ))
                            (rhs (list (powerline-raw "%4l")
                                       (powerline-raw ":")
                                       (powerline-raw "%2c")
                                       (powerline-raw " | ")                                  
                                       (powerline-raw "%6p")
                                       (powerline-raw " ")
                                       )))
                       (concat (powerline-render lhs)
                               (powerline-fill nil (powerline-width rhs)) 
                               (powerline-render rhs)))))))
  (powerline-my-theme)
  

(defun make/set-face (face-name fg-color bg-color weight)
  (make-face face-name)
  (set-face-attribute face-name nil
                      :foreground fg-color :background bg-color :box nil :weight weight))
(make/set-face 'mode-line-1-fg "#282C34" "#EF8300" 'bold)
(make/set-face 'mode-line-2-fg "#AAAAAA" "#2F343D" 'bold)
(make/set-face 'mode-line-1-arrow  "#AAAAAA" "#3E4451" 'bold)
(make/set-face 'mode-line-2-arrow  "#AAAAAA" "#3E4451" 'bold)

  )
(use-package popup
  :ensure t)
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

  (bind-key* "C-n" 'neotree-refresh)
  
  (neotree)
  )
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program shell-file-name)
  (bind-key "C-x t" 'multi-term)
  )
;(use-package monokai-theme
;  :ensure t
;  :config
;  (load-theme 'monokai t)
;  )
;(use-package darcula-theme
;  :ensure t
;  :init (load-theme 'darcula t)
;  )
;(use-package color-theme-sanityinc-tomorrow
;  :ensure t
;  :init (load-theme 'sanityinc-tomorrow-eighties)
                                        ;)
(use-package nord-theme
  :ensure t
  :init (load-theme 'nord t)
  )

(use-package tabbar
  :ensure t
  :config
  (tabbar-mwheel-mode nil)                  ;; マウスホイール無効
  (setq tabbar-buffer-groups-function nil)  ;; グループ無効
  (setq tabbar-use-images nil)              ;; 画像を使わない
  ;;----- 左側のボタンを消す
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
                   (cons "" nil))))


  ;;----- タブのセパレーターの長さ
  (setq tabbar-separator '(1.0))
  ;;----- 表示するバッファ
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

(use-package markdown-preview-mode
  :ensure t
  :config
  (setq markdown-preview-stylesheets (list "github.css"))
  (setq markdown-command "multimarkdown")
  (bind-key "C-c p" 'markdown-preview-mode markdown-mode-map)
  )
(use-package go-mode
  :ensure t)
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode) ; flycheck-pos-tip
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
  )
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
(use-package smooth-scroll
  :ensure t)
(use-package anzu
  :ensure t
  :init (global-anzu-mode +1)
  )
(use-package highlight-symbol
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (setq highlight-symbol-idle-delay 1.0)
  )
(use-package helm
  :ensure t
  :init
  (helm-mode t)
  :config
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

  (custom-set-variables
   '(helm-mini-default-sources '(helm-source-buffers-list
                                 helm-source-recentf
                                 helm-source-ls-git
                                 helm-source-files-in-current-dir
                                 helm-source-emacs-commands-history
                                 helm-source-emacs-commands
                                 )))
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
   )
  (setq helm-default-display-buffer-functions '(display-buffer-in-atom-window))
  (bind-key* "M-x" 'helm-mini)
  (bind-key* "C-x RET" 'helm-mini)
  (bind-key* "C-x f" 'helm-find-files)
  (bind-key* "C-x b" 'helm-buffers-list)
  (bind-key* "C-x a" 'helm-do-grep-ag)
  )
(use-package flycheck-pos-tip
  :ensure t)
(use-package company-go
  :ensure t)
(use-package json-mode
  :ensure t)
(use-package php-mode
  :ensure t)
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  )

(use-package glsl-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.shader$" . glsl-mode))
  )

(use-package company-glsl
  :ensure t
  )

(use-package terraform-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tf$" . terraform-mode))
  )

(use-package dockerfile-mode
  :ensure t)
(use-package nginx-mode
  :ensure t)
(use-package helm-ag
  :ensure t
  )
(use-package helm-git-grep
  :ensure t
  :config
  (global-set-key (kbd "C-x s") 'helm-git-grep)
  (global-set-key (kbd "C-x g") 'helm-browse-project)
  )
(use-package helm-ls-git
  :ensure t)
(use-package groovy-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\Jenkinsfile$" . groovy-mode))
  (defun my-c-mode-hook () 
    (setq indent-tabs-mode nil 
          c-basic-offset 4)) 
  (add-hook 'c-mode-common-hook 'my-c-mode-hook) 

  )

(use-package git-gutter+
  :ensure t
  :init (global-git-gutter+-mode)
  )

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

; PATHをシェルから引き継ぐ
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize)
  )

; フォースを信じろ
(use-package company-jedi
  :ensure t
  )

(use-package jedi
  :ensure t
  :config
  (setq jedi:complete-on-dot t)
  (bind-key "C-x d" 'jedi:goto-definition jedi-mode-map)
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  )

(use-package dumb-jump
  :config
  ;(setq dumb-jump-selector 'ivy)
  (setq dumb-jump-selector 'helm)
  (bind-key "C-x d" 'dumb-jump-go)
  :ensure t
  )

(use-package swiper
  :ensure t
  :config
  (bind-key* "C-s" 'swiper)
  )

(use-package plantuml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (setq plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.12/libexec/plantuml.jar")
  (setq plantuml-java-options "")
  (setq plantuml-output-type "png")
  (setq plantuml-options "-charset UTF-8")
  )


;(use-package golden-ratio
;  :ensure t
;  :init (golden-ratio-mode t)
;  :config
;  (add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
;  )

;(use-package dashboard
;  :ensure t
;  :config
;  (dashboard-setup-startup-hook)
;  )

;;   ###  キーバインド設定 ###

;; ウインドウ分割
(bind-key* "M-d" 'split-window-vertically)
(bind-key* "C-d" 'other-window)
(bind-key* "C-f" 'other-window)
(bind-key* "C-<tab>" 'other-window)

(bind-key* "C-x l" 'windmove-left)
(bind-key* "C-x :" 'windmove-right)
(bind-key* "C-x p" 'windmove-up)
(bind-key* "C-x ;" 'windmove-down)

;; ウインドウ最大化

;; 画面のが最大化されている or NOTの状態を保持
(defvar is-window-maximized nil)

;; 1. 最大化されている場合
;;  -> `balance-windows` で画面のバランスを調整
;; 2. 最大化されていない場合
;;  -> `maximize-window` で画面を最大化

(defun window-toggle-max ()
  (interactive) ;; 補足あり
  (progn
    (if is-window-maximized
      (balance-windows)
    (maximize-window))
  (setq is-window-maximized (not is-window-maximized))))

(bind-key* "C-x m" 'window-toggle-max)

;; スキップ移動
(bind-key* "M-<down>" (kbd "C-u 5 <down>"))
(bind-key* "M-<up>" (kbd "C-u 5 <up>"))
(bind-key* "M-<right>" 'forward-word)
(bind-key* "M-<left>" 'backward-word)
;; たーみなる
(bind-key* "ESC <down>" (kbd "C-u 5 <down>"))
(bind-key* "ESC <up>" (kbd "C-u 5 <up>"))



;; コピー
(bind-key* "C-q" 'copy-region-as-kill)

(bind-key* "C-x C-b" 'buffer-menu)
;(bind-key* "C-b" 'list-buffers)

;; バッファ移動
(bind-key* "C-b" 'previous-buffer)

;; eww
(bind-key* "C-x w" 'eww)

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

;; window resize
(bind-key* "C-x r" 'window-resizer)


;;   ###  ファイルホック ###

;; sql-mode
(add-to-list 'auto-mode-alist '("\\.sql?$"     . sql-mode))

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue?$"     . web-mode))

;;   ### diffを見やすく ###
;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)

;; ### CHEAT COMMAND ###
(defvar cheat-root "~/.order66/cheat/")

(defun cheat()
  (interactive)
  (view-file (concat cheat-root "cheat.md"))
  )

(defun cheat-tmux()
  (interactive)
  (view-file (concat cheat-root "cheat-tmux.md"))
  )

(defun cheat-mysql()
  (interactive)
  (view-file (concat cheat-root "cheat-mysql.md"))
  )

(defun cheat-go()
  (interactive)
  (view-file (concat cheat-root "cheat-go.md"))
  )

(defun cheat-rsync()
  (interactive)
  (view-file (concat cheat-root "cheat-rsync.md"))
  )



