
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
;;(define-key markdown-mode-map (kbd "S-<tab>") nil)

(setq mac-option-key-is-meta t)

;; はみ出した表示をウインドウの右端で折り返さない
(setq-default truncate-lines nil)
(setq-default truncate-partial-width-windows nil)

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
;;	  )
;;	  initial-frame-alist
;;	  )
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
  (custom-set-faces
   '(mode-line ((t (:foreground "#f9f9f9" :background "#AD1457" :box nil :height 140))))
   '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil :height 140))))
   '(powerline-active1 ((t (:foreground "#f9f9f9" :background "#666666" :box nil :height 140))))
   '(powerline-active2 ((t (:foreground "#f9f9f9" :background "#AD1457" :box nil :height 140))))
   '(powerline-active0 ((t (:foreground "#f9f9f9" :background "#880E4F" :box nil :height 140))))
   )
  )
(use-package popup
  :ensure t)
(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-show-hidden-files t)
  (eval-after-load "neotree"
    '(progn
       (define-key neotree-mode-map (kbd "C-i") nil)
       ))
  (global-set-key (kbd "C-c c") 'neotree-create-node)
  (global-set-key (kbd "C-c d") 'neotree-delete-node)
  (global-set-key (kbd "C-c r") 'neotree-rename-node)
  (global-set-key (kbd "C-c p") 'neotree-copy-node)
  (global-set-key (kbd "C-c s") 'neotree-stretch-toggle)
  (global-set-key (kbd "C-c t") 'neotree-toggle)

  (neotree)
  )
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program shell-file-name)
  )
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t)
  )
(use-package markdown-mode
  :ensure t)
(use-package go-mode
  :ensure t)
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)                                        ; flycheck-pos-tip
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
  )
(use-package company
  :ensure t
  :config
  (global-company-mode) ; 全バッファで有効にする
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
  :config
  (global-anzu-mode +1)
  )
(use-package helm
  :ensure t
  :config
  (helm-mode 0)
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
                                 helm-source-files-in-current-dir
                                 helm-source-emacs-commands-history
                                 helm-source-emacs-commands
                                 helm-source-ls-git
                                 )))
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)

  (define-key global-map (kbd "M-x") 'helm-mini)
  (define-key global-map (kbd "C-x RET") 'helm-mini)

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
(use-package dockerfile-mode
  :ensure t)
(use-package nginx-mode
  :ensure t)
(use-package helm-git-grep
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'helm-git-grep)
  (eval-after-load 'helm
    '(define-key helm-map (kbd "C-x g") 'helm-git-grep-from-helm))

  (global-set-key (kbd "C-x l") 'helm-browse-project)

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

;;   ###  キーバインド設定 ###

;; ウインドウ分割
(global-set-key (kbd "C-d") 'split-window-horizontally)
(global-set-key (kbd "M-d") 'split-window-vertically)
(global-set-key (kbd "C-f") 'other-window)


;; スキップ移動

(global-set-key (kbd "M-<down>") (kbd "C-u 5 <down>"))
(global-set-key (kbd "M-<up>") (kbd "C-u 5 <up>"))
(global-set-key (kbd "M-<right>") 'forward-word)
(global-set-key (kbd "M-<left>") 'backward-word)

;; コピー
(global-set-key (kbd "C-q") 'copy-region-as-kill)
(global-set-key (kbd "C-S-w") 'copy-region-as-kill)

;; バッファリストを別ウインドウで開かないようにする
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-b") 'buffer-list)

;; eww
(global-set-key (kbd "C-c g") 'eww)

;; window resize
(global-set-key (kbd "C-x r") 'window-resizer)


;;   ###  ファイルホック ###

;; sql-mode
(add-to-list 'auto-mode-alist '("\\.sql?$"     . sql-mode))

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue?$"     . web-mode))


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

