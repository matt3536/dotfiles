;;;;load-pathes

;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths) (let (path) (dolist (path paths paths) (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path "lisp" "site-lisp")

;;;;  package.el

;; カスタムファイル(自動で書き込まれるファイル)を別ファイルにする
(setq custom-file (locate-user-emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;package.elを有効化
(require 'package)
;;パッケージリポジトリ
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
;;パッケージを読み込む
(package-initialize) 

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;;;;  General / Apperance

;; C-mで改行＋インデントを行う
(global-set-key (kbd "C-m") 'newline-and-indent)

;; C-tでウィンドウを切り替える。初期値はtranspose-chars
(global-set-key (kbd "C-t") 'other-window)

;; C-hを<BACKSPACE>に置き換え、"C-x ?"にヘルプコマンドを入れ替える
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key global-map (kbd "C-x ?") 'help-command)

;; 行番号を左側に常に表示p
(global-linum-mode t)

;; TAB幅
(setq-default tab-width 4)
;; インデントにTAB文字を使用しない
(setq-default indent-tabs-mode nil)

;; メニューバーの非表示
(menu-bar-mode -1)

;; ツールバーの非表示
(tool-bar-mode -1)

;; *.~などのバックアップファイルを作成しない
(setq make-backup-files nil)

;; .#*などのバックアップファイルを作成しない
(setq auto-save-file-name-transforms
      '((".*" "~/tmp/" t)))

;; 'save-xxxx'などのファイルを作成しない
(setq auto-save-list-file-prefix nil)

;;;;  use-packages

(use-package swiper
  :bind
  ("M-s M-s" . swiper-thing-at-point))
 
(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1.0))

(use-package hide-mode-line
  :hook
  ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))

(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (doom-modeline-def-modeline 'main-line
  '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
  '(misc-info github persp-name lsp minor-modes input-method buffer-encoding major-mode process vcs checker)))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))


(use-package paren
   :ensure nil
   :hook
   (after-init . show-paren-mode)
   :custom-face
   (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))))
   :custom
   (show-paren-style 'mixed)
   (show-paren-when-point-inside-paren t)
   (show-paren-when-point-in-periphery t))

(use-package neotree
    :after
    projectile
    :commands
    (neotree-show neotree-hide neotree-dir neotree-find)
    :custom
    (neo-theme 'nerd2)
    :bind
    ("<f9>" . neotree-projectile-toggle)
    :preface
    (defun neotree-projectile-toggle ()
      (interactive)
      (let ((project-dir
         (ignore-errors
         ;;; Pick one: projectile or find-file-in-project
           (projectile-project-root)
           ))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
         (neo-global--window-exists-p))
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
        (neotree-dir project-dir))
        (if file-name
        (neotree-find file-name)))))))

(use-package highlight-indent-guides
    :diminish
    :hook
    ((prog-mode yaml-mode) . highlight-indent-guides-mode)
    :custom
    (highlight-indent-guides-auto-enabled t)
    (highlight-indent-guides-responsive t)
    (highlight-indent-guides-method 'character)) ; column

 (use-package beacon
    :custom
    (beacon-color "yellow")
    :config
    (beacon-mode 1))

 (use-package dashboard
   :diminish
   (dashboard-mode page-break-lines-mode)
   :custom
   (dashboard-startup-banner "~/.emacs/dashboard.txt")
   (dashboard-items '((recents . 15)
              (projects . 5)
              (bookmarks . 5)
              (agenda . 5)))
   :hook
   (after-init . dashboard-setup-startup-hook)
   :config
   (add-to-list 'dashboard-items '(agenda) t))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :init
  :config
  )
