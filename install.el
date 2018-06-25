;; package.elを有効化
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(fset 'package-desc-vers 'package--ac-desc-version)
(package-initialize)
;; パッケージ情報の更新
(package-refresh-contents)

;; インストールするパッケージの指定
(defvar my-packages
  '(
	markdown-mode
	multi-term
	company
	all-the-icons
	neotree
	flycheck
	web-mode
	go-mode
	powerline
	undo-tree
	smooth-scroll
	monokai-theme
	)
  )

;; パッケージインストール
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))
