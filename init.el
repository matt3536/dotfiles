(require 'profiler)
(profiler-start 'cpu)

;;;; load-pathes

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

;;;;;
;;;;;  General / Apperance
;;;;;

;;;; Server
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;;;; Editing

;;; Language Format
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8)

;;; Quick Startup
(setq inhibit-startup-screen t)		  ; Hide Startup Screen
(setq inhibit-startup-message t)   	  ; Hide Startup message
(setq inhibit-startup-echo-area-message t) ; Hide Startup echo area message
(setq initial-scratch-message nil)	  ; Hide scratch buufer's message

(defun display-startup-echo-area-message ()
  (message ""))

;;; Options
(setq frame-title-format nil) 		  ; Frame Title
(setq ring-bell-function 'ignore) 	  ; Error Sound
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq delete-by-moving-to-trash t)    ; Deleting files go to OS's trash folder
(setq make-backup-files nil)          ; Forbide to make backup files (ex. *.~)
(setq auto-save-default nil)          ; Disable auto save (ex. .#*)
(setq set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again
(setq mark-ring-max 16)				  ; MAX mark-rings :default = 16 :command = C-x C-<SPC>
(setq track-eol t)					  ; Keep cursor at end of lines.
(setq line-move-visual nil)			  ; To be required by track-eol
(setq-default kill-whole-line t)	  ; Kill line including '\n'
(setq-default indent-tabs-mode nil)	  ; Use space for Indent
(setq-default tab-width 4)			  ; Tab Width :default = 8
(defalias 'yes-or-no-p 'y-or-n-p)	  ; Make the answer in Interactive mode "y-or-n"
(global-display-line-numbers-mode t)  ; Line Number
(menu-bar-mode -1)                    ; Menu Bar 
(scroll-bar-mode -1)                  ; Scroll Bar
(tool-bar-mode -1)                    ; Tool Bar 

;; Make the answer in Minibuffer ascii mode
(when (functionp 'mac-auto-ascii-mode)
  (mac-auto-ascii-mode 1))			  

;; Delete selection if insert someting
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Delete blanks
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;;;; Smart parens

;; Automatically parens
(use-package smartparens
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap)))

;;;; History

;; Cursol History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Recent Files
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 20000)
  (recentf-auto-cleanup 'never)		; D'ont delete non-exitent files
  :preface
  (defun recentf-save-list-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if (fboundp 'shut-up)
          (shut-up (recentf-save-list))
        (recentf-save-list)))
    (message ""))
  (defun recentf-cleanup-silence ()
     (interactive)
     (let ((message-log-max nil))
      (if shutu-p
          (shut-up (recentf-cleanup))
        (recentf-cleanup)))
    (message ""))
  :hook
  (focus-out-hook . (recentf-save-list-silence recentf-cleanup-silence)))

;;;;;
;;;;; GUI/Fonts
;;;;;

;;;; Fonts
(set-face-attribute 'default nil :family "Ricty Diminished" :height 130)

;;;; Icons
(use-package all-the-icons
  :if (display-graphic-p)
  :config (unless (find-font (font-spec :name "all-the-icons"))
            (all-the-icons-install-fonts t)))

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package all-the-icons-ivy
  :after (ivy ivy-rich)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

;;;; Point
(use-package popwin)
(use-package point-history
  :load-path "~/src/github.com/blue0513/point-history"
  :config
  (point-history-mode t))

;;;;;
;;;;; Key-bindings
;;;;;

;;;; Global
(global-set-key (kbd "C-h")     'backward-delete-char)
(global-set-key (kbd "C-m") 	'electric-newline-and-maybe-indent)
(global-set-key (kbd "C-j") 	'newline-and-indent)
(global-set-key (kbd "C-o")		'other-window)
(global-set-key (kbd "C-x ?")   'help-command)
(global-set-key (kbd "C-c e")   'ediff-buffers)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-r")     'rename-file)
(global-set-key (kbd "C-c l")   'toggle-truncate-lines)

;;;; Which-key
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;;;; Hydra
(use-package hydra
  :ensure t)

;;;; Undo/Redo
(use-package undo-tree
  :delight
  :bind ("C-/" . undo-tree-redo)
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t))

;;;;;
;;;;; Search/Replace
;;;;;

;;;; projectile
(use-package projectile
  :diminish
  :bind
  ("M-o p" . counsel-projectile-switch-project)
  :config
  (projectile-mode +1))

;;;; Wgrep
(use-package wgrep
  :defer t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;;;; rg
(use-package rg
  :bind
  ("C-c s" . rg)
  ("M-s r" . rg-project))

;;;; ivy-swiper-counsel
(use-package counsel
  :after ivy
  :delight
  :bind
  (("M-x" . counsel-M-x)
  ("M-y" . counsel-yank-pop)
  ("C-x C-l" . counsel-find-library)
  ("C-x C-r" . counsel-recentf)
  ("C-x C-b" . counsel-ibuffer)
  ("M-o f" . counsel-fzf)
  ("M-o c" . counsel-rg))
  :custom
  (counsel-yank-pop-height 20)
  (enable-recursive-minibuffers t)
  (swiper-action-recenter t)
  (counsel-rg-base-command "rg -S -M 150 --no-heading --line-number --color never %s")
  :config
  (counsel-mode t)
  (advice-add
  'counsel--yank-pop-format-function
  :override
  (lambda (cand-pairs)
   (ivy--format-function-generic
     (lambda (str)
      (mapconcat
       (lambda (s)
         (ivy--add-face (concat (propertize "┃ " 'face `(:foreground "#61bfff")) s) 'ivy-current-match))
       (split-string
        (counsel--yank-pop-truncate str) "\n" t)
        "\n"))
     (lambda (str)
      (counsel--yank-pop-truncate str))
     cand-pairs
     counsel-yank-pop-separator)))
  ;; NOTE: this variable do not work if defined in :custom
  (setq counsel-yank-pop-separator
      (propertize "\n────────────────────────────────────────────────────────\n"
          'face `(:foreground "#6272a4")))
    ;; Ivy integration for Projectile
    (use-package counsel-projectile
      :config (counsel-projectile-mode 1)))

(use-package ivy
  :delight
  :after ivy-rich
  :bind
  (("C-x b" . ivy-switch-buffer)
  ("C-x B" . ivy-switch-buffer-other-window)
  ("C-x C-b" . ivy-switch-buffer-other)
  ("M-H"   . ivy-resume)
  :map ivy-minibuffer-map
  ("C-w" . ivy-backward-kill-word)
  ("C-j" . ivy-immediate-done)
  ("RET" . ivy-alt-done)
  ("C-h" . ivy-backward-delete-char)
  :map ivy-switch-buffer-map
  ("C-k"   . ivy-switch-buffer-kill))
  :custom
  (ivy-use-selectable-prompt t)
  (ivy-case-fold-search-default t)
  (ivy-re-builders-alist '((t . ivy--regex-plus)))
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (ivy-height 35)
  :config
  ;; Enhance fuzzy matching
  (use-package flx)
  ;; Enhance M-x
  (use-package amx)
  ;; ghq
  (use-package ivy-ghq
   :load-path "~/src/github.com/analyticd/ivy-ghq"
   :commands (ivy-ghq-open)
   :bind
   ("M-o g" . ivy-ghq-open-and-fzf)
   :custom
   (ivy-ghq-short-list t)
   :preface
   (defun ivy-ghq-open-and-fzf ()
    (interactive)
    (ivy-ghq-open)
    (counsel-fzf))))

(use-package ivy-pass
  :after ivy
  :commands ivy-pass)

(use-package ivy-rich
  :defer 0.1
  :preface
  (defun ivy-rich-branch-candidate (candidate)
    "Displays the branch candidate of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s%s"
                (propertize
                 (replace-regexp-in-string abbreviated-home-dir "~/"
                                           (file-name-directory
                                            (directory-file-name candidate)))
                 'face 'font-lock-doc-face)
                (propertize
                 (file-name-nondirectory
                  (directory-file-name candidate))
                 'face 'success)))))

  (defun ivy-rich-compiling (candidate)
    "Displays compiling buffers of the candidate for ivy-rich."
    (let* ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate)
              (not (magit-git-repo-p candidate)))
          ""
        (if (my/projectile-compilation-buffers candidate)
            "compiling"
          ""))))

  (defun ivy-rich-file-group (candidate)
    "Displays the file group of the candidate for ivy-rich"
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((group-id (file-attribute-group-id (file-attributes candidate)))
               (group-function (if (fboundp #'group-name) #'group-name #'identity))
               (group-name (funcall group-function group-id)))
          (format "%s" group-name)))))

  (defun ivy-rich-file-modes (candidate)
    "Displays the file mode of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s" (file-attribute-modes (file-attributes candidate))))))

  (defun ivy-rich-file-size (candidate)
    "Displays the file size of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let ((size (file-attribute-size (file-attributes candidate))))
          (cond
           ((> size 1000000) (format "%.1fM " (/ size 1000000.0)))
           ((> size 1000) (format "%.1fk " (/ size 1000.0)))
           (t (format "%d " size)))))))

  (defun ivy-rich-file-user (candidate)
    "Displays the file user of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((user-id (file-attribute-user-id (file-attributes candidate)))
               (user-name (user-login-name user-id)))
          (format "%s" user-name)))))

  (defun ivy-rich-switch-buffer-icon (candidate)
    "Returns an icon for the candidate out of `all-the-icons'."
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode :height 0.9)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode :height 0.9)
          icon))))
  :config
  (plist-put ivy-rich-display-transformers-list
             'counsel-find-file
             '(:columns
               ((ivy-rich-candidate               (:width 73))
                (ivy-rich-file-user               (:width 8 :face font-lock-doc-face))
                (ivy-rich-file-group              (:width 4 :face font-lock-doc-face))
                (ivy-rich-file-modes              (:width 11 :face font-lock-doc-face))
                (ivy-rich-file-size               (:width 7 :face font-lock-doc-face))
                (ivy-rich-file-last-modified-time (:width 30 :face font-lock-doc-face)))))
  (plist-put ivy-rich-display-transformers-list
             'counsel-projectile-switch-project
             '(:columns
               ((ivy-rich-branch-candidate        (:width 80))
                (ivy-rich-compiling))))
  (plist-put ivy-rich-display-transformers-list
             'ivy-switch-buffer
             '(:columns
               ((ivy-rich-switch-buffer-icon       (:width 2))
                (ivy-rich-candidate                (:width 40))
                (ivy-rich-switch-buffer-size       (:width 7 :height 70))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 20 :face warning)))
               :predicate (lambda (cand) (get-buffer cand))))
  (ivy-rich-mode 1))

;;;; swiper
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         :map swiper-map
         ("M-%" . swiper-query-replace))
  		 ("M-s M-s" . swiper-thing-at-point))

;;;; anzu
; Highlight matches in the buffer when searching
(use-package anzu
  :diminish
  :bind
  ("C-r"   . anzu-query-replace-regexp)
  ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook
  (after-init . global-anzu-mode))

;;;; migemo
(use-package migemo
  :custom
  (migemo-command "/usr/local/bin/cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-coding-system 'utf-8-unix)
  (migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  :config
  (migemo-init)
  (use-package avy-migemo
    :after swiper
    :config
    ;; (avy-migemo-mode 1)
    (require 'avy-migemo-e.g.swiper))
  )

;;;;;
;;;;; Cursor
;;;;;


;;;; Multiple-Cursor

;;;; Avy/Ace
(use-package avy
 :bind
 ("C-'"   . avy-resume)
 ("C-:"   . avy-goto-char-2-below)
 ("C-;"   . avy-goto-char)
 :preface
 ;; fixed cursor scroll-up
 (defun scroll-up-in-place (n)
  (interactive "p")
   (forward-line (- n))
   (scroll-down n))
 ;; fixed cursor scroll-down
 (defun scroll-down-in-place (n)
  (interactive "p")
   (forward-line n)
   (scroll-up n))
  ;; yank inner sexp
  (defun yank-inner-sexp ()
   (interactive)
    (backward-list)
    (mark-sexp)
    (copy-region-as-kill (region-beginning) (region-end)))
 :config
  (when (eq system-type 'darwin)
   (progn
    (global-set-key (kbd "C-:") 'avy-goto-char)
    (global-set-key (kbd "C-;") 'avy-goto-char-2-below))))

(use-package avy-zap
 :bind
 ("M-z" . avy-zap-to-char-dwim)
 ("M-z" . avy-zap-up-to-char-dwim))

(use-package ace-window
 :functions hydra-frame-window/body
 :bind
 ("C-c m" . toggle-window-maximize)
 ("C-M-o" . hydra-frame-window/body)
 :custom
 (aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
 :custom-face
 (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
 :preface
 (defvar is-window-maximized nil)
 (defun my/toggle-window-maximize ()
   (interactive)
   (progn
    (if is-window-maximized
      (balance-windows)
     (maximize-window))
    (setq is-window-maximized
       (not is-window-maximized))))
 :config
 (use-package rotate
  :bind
  ("M-o SPC" . rotate-layout))
 (with-eval-after-load 'hydra
  (defhydra hydra-frame-window (:color gray)
    ("K" kill-current-buffer "kill-current-buffer" :exit t)
    ("D" kill-buffer-and-window "kill-buffer-and-window" :exit t)
    ("F" toggle-frame-fullscreen "Fullscreen")
    ("i" ace-window "ace-window")
    ("s" ace-swap-window "ace-swap" :exit t)
    ("d" ace-delete-window "ace-delete")
    ("m" toggle-window-maximize "toggle-window-maximize" :exit t)
    ("=" text-scale-decrease "out")    
    ("+" text-scale-increase "in")    
    ("-" split-window-vertically "split-window-V")    
    ("/" split-window-horizontally "split-window-H")
    ("h" shrink-window-horizontally "shrink-window-H")    
    ("k" shrink-window "shrink-window")    
    ("j" enlarge-window "enlarge-window")
    ("l" enlarge-window-horizontally "enlarge-window-H")    
    ("," previous-buffer "P-buffer")    
    ("." next-buffer "N-buffer")    
    ("o" other-window "ther-buffer")        
    ("r" counsel-recentf "recentf" :exit t)    
    ("<SPC>" rotate-layout "rotate-layout")
    ("q" nil "EXIT"))))

;;;; Smart Move
(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;;;;;
;;;;; Checker
;;;;;

;;;; flymake
(use-package flymake
  :hook
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  :config
  (flymake-mode t))

;;;; flyspell

;;;;;
;;;;; Project
;;;;;

;;;; persp
(use-package persp-mode
  :disabled
  :diminish
  :defines ivy-sort-functions-alist
  :commands (get-current-persp persp-contain-buffer-p persp-add persp-by-name-and-exists)
  :hook ((after-init . persp-mode)
         (emacs-startup . toggle-frame-maximized))
  :custom
  (persp-keymap-prefix (kbd "C-x p"))
  (persp-nil-name "default")
  (persp-set-last-persp-for-new-frames nil)
  (persp-auto-resume-time 0)
  :config
  ;; NOTE: Redefine `persp-add-new' to address.
  ;; Issue: Unable to create/handle persp-mode
  ;; https://github.com/Bad-ptr/persp-mode.el/issues/96
  ;; https://github.com/Bad-ptr/persp-mode-projectile-bridge.el/issues/4
  ;; https://emacs-china.org/t/topic/6416/7
  (defun* persp-add-new (name &optional (phash *persp-hash*))
    "Create a new perspective with the given `NAME'. Add it to `PHASH'.
  Return the created perspective."
    (interactive "sA name for the new perspective: ")
    (if (and name (not (equal "" name)))
        (destructuring-bind (e . p)
            (persp-by-name-and-exists name phash)
          (if e p
            (setq p (if (equal persp-nil-name name)
                        nil (make-persp :name name)))
            (persp-add p phash)
            (run-hook-with-args 'persp-created-functions p phash)
            p))
      (message "[persp-mode] Error: Can't create a perspective with empty name.")
      nil))

  ;; Ignore temporary buffers
  (add-hook 'persp-common-buffer-filter-functions
            (lambda (b) (or (string-prefix-p "*" (buffer-name b))
                       (string-prefix-p "magit" (buffer-name b)))))

  
  ;; Integrate IVY
  (with-eval-after-load "ivy"
    (add-hook 'ivy-ignore-buffers
              #'(lambda (b)
                  (when persp-mode
                    (let ((persp (get-current-persp)))
                      (if persp
                          (not (persp-contain-buffer-p b persp))
                        nil)))))

    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch  . nil))))))

;;;;;
;;;;; Complition
;;;;;

;;;; Yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :custom (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :hook (after-init . yas-global-mode))

;;;; Company
(use-package company
  :diminish company-mode
  :bind
  (:map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("<tab>" . company-complete-common-or-cycle)
   :map company-search-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next))
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  :hook
  (after-init . global-company-mode)
  (plantuml-mode . (lambda () (set (make-local-variable 'company-backends)
                            '((company-yasnippet
                               ;; company-dabbrev
                               )))))
  ((go-mode
    c++-mode
    c-mode
    objc-mode
    python-mode) . (lambda () (set (make-local-variable 'company-backends)
                            '((company-capf
                               company-yasnippet
                               company-files
                               ;; company-dabbrev-code
                               )))))
  :config
  (use-package company-box
   :delight
   :hook (company-mode . company-box-mode)))

;;;;;
;;;;; Git
;;;;;

;;;; TimeMachine
(use-package git-timemachine
  :bind ("M-g t" . git-timemachine-toggle))

;;;; DiffView
(use-package diffview
  :commands (diffview-region diffview-current)
  :preface
  (defun my/diffview-dwim ()
    (interactive)
    (if (region-active-p)
        (diffview-region)
      (diffview-current)))
  :bind ("M-g v" . diffview-dwim))

;;;; Magit
(use-package magit
  :custom
  (magit-auto-revert-mode nil)
  :bind
  ("M-g s" . magit-status))

;;;; GitGutter
(use-package git-gutter
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-")
  :custom-face
  (git-gutter:modified ((t (:background "#f1fa8c"))))
  (git-gutter:added    ((t (:background "#50fa7b"))))
  (git-gutter:deleted  ((t (:background "#ff79c6"))))
  :config
  (global-git-gutter-mode +1))

;;;; Git Remote
(use-package browse-at-remote
  :bind ("M-g r" . browse-at-remote))

;;;; PullRequest
(use-package forge
  :after magit)

;;;; Smerge
(use-package smerge-mode
  :diminish
  :preface
  (with-eval-after-load 'hydra
    (defhydra smerge-hydra
      (:color gray :hint nil :post (smerge-auto-leave))
      "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("ZZ" (lambda ()
              (interactive)
              (save-buffer)
              (bury-buffer))
       "Save and bury buffer" :color blue)
      ("q" nil "cancel" :color blue)))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-hydra/body))))))

;;;;;
;;;;; Tools
;;;;;

;;;; Ediff
;; コントロール用のバッファを同一フレーム内に表示する
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;;; Google Translater
(use-package google-translate
  :bind
  ("M-o t" . google-translate-at-point)
  ("M-o T" . google-translate-at-point-reverse)
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja"))

;;;;;
;;;;; Language
;;;;;

;;;; LSP
;; Installed LSP Server
;; * C/C++  -- ccls(LSP LLDB server)
;; * Go     -- VSCode.Go(DAP delve server)
;; *        -- VSCode.NativeDebug(DAP GDB-LLDB server)
;; *        -- gopls(LSP server)
;; * Python -- pyright(LSP server)
(use-package lsp-mode
  :hook
  ((c-mode c++-mode go-mode python-mode) . lsp)
  :custom
  ;; debug
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  ;; general
  (lsp-auto-guess-root t)
  ;; (lsp-document-sync-method 'lsp-sync-incremental) ;; none, full, incremental, or nil
  (lsp-document-sync-method 2)
  (lsp-response-timeout 10)
  (lsp-completion-provider :capf)
  (lsp-prefer-flymake t) ;; t(flymake), nil(lsp-ui), or :none
  ;;company
  (lsp-prefer-capf t)
  :bind
  (:map lsp-mode-map
        ("C-c r" . lsp-rename))
  :config
  ;;(require 'lsp-clients)
  ;;LSP UI tools
  (use-package lsp-ui
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable nil) ;; toggle-lsp-ui-doc
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature nil)
    (lsp-ui-doc-position 'at-point) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width 120)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit t)
    ;; lsp-ui-flycheck
    (lsp-ui-flycheck-enable nil)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable nil)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics nil)
    (lsp-ui-sideline-show-code-actions t)
    (lsp-ui-sideline-code-actions-prefix "")
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable t)
    (lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-peek-height 20)
    (lsp-ui-peek-list-width 50)
    (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
    :preface
    (defun my/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
         (lsp-ui-doc-mode 1)))
    :bind
    (:map lsp-mode-map
    ("C-c C-r" . lsp-ui-peek-find-references)
    ("C-c C-j" . lsp-ui-peek-find-definitions)
    ("C-c i"   . lsp-ui-peek-find-implementation)
    ("C-c m"   . lsp-ui-imenu)
    ("C-c s"   . lsp-ui-sideline-mode)
    ("C-c d"   . ladicle/toggle-lsp-ui-doc))
    :hook
    (lsp-mode . lsp-ui-mode))
  ;; DAP
  (use-package dap-mode
    :custom
    (dap-lldb-debug-program `("~/.vscode/extensions/lanza.lldb-vscode-0.2.3/bin/darwin/bin/lldb-vscode"))
    (dap-go-debug-program `("node" "~/.vscode/extensions/golang.go-0.25.0/dist/debugAdapter.js"))
    (dap-gdb-lldb-debug-program `("node" "~/.vscode/extensions/webfreak.debug-0.25.0/out/src/gdb.js"))
    :config
    (require 'dap-hydra)
    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    (require 'dap-go)
    (dap-mode 1)
    (use-package dap-ui
     :ensure nil
     :config
     (dap-ui-mode 1))))

;;;; Golang
(use-package go-mode
  :mode "\\.go\\'"
  :if (executable-find "gopls")
  :custom (gofmt-command "goimports")
  :bind (:map go-mode-map
         ("C-c C-n" . go-run)
         ("C-c ."   . go-test-current-test)
         ("C-c f"   . go-test-current-file)
         ("C-c a"   . go-test-current-project))
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (use-package gotest)
  (use-package go-tag
    :config (setq go-tag-args (list "-transform" "camelcase"))))
    
;;;; C/C++
(use-package cc-mode
  :bind (:map c-mode-base-map
         ("C-c c" . compile))
  :hook (c-mode-common . (lambda ()
                            (c-set-style "bsd")
                            (setq tab-width 4)
                            (setq c-base-offset 4))))
(use-package ccls
  :custom
  (ccls-executable "/usr/local/bin/ccls")
  (ccls-sem-highlight-method 'font-lock)
  :config
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

;;;; Java
;(use-package lsp-java
;  :after lsp
;  :hook (java-mode . lsp)
;  :custom (lsp-java-server-install-dir
;           (expand-file-name (format "%s/eclipse.jdt.ls/server" xdg-lib))))

;;;; Python

(use-package lsp-pyright
 :if (executable-find "pyright-langserver")
 :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                           (lsp))))  ; or lsp-deferred

(use-package python-mode
  :mode "\\.py\\'"
  :config
  (use-package py-isort
  :hook ((python-mode . pyvenv-mode)
         (before-save . py-isort-before-save)))
  (use-package pyenv-mode
  :hook ((python-mode . pyenv-mode)))
  ;; Formatter
  (use-package blacken
  :hook (python-mode . blacken-mode)
  :custom (blacken-line-length 79)))

;;;; PlantUML
(use-package plantuml-mode
  :custom
  (plantuml-jar-path "~/.emacs.d/plantuml.jar")
  :mode "\\.uml\\'")


;;;; Org
(use-package org
  :custom
  (org-src-fontify-natively t)
  (private-directory "~/Private/")
  (task-file (concat "~/Private" "task.org"))
  (schedule-file (concat "~/Private" "schedule.org"))
  (org-directory "~/Private/")
  (org-plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (org-confirm-babel-evaluate nil)
  (org-clock-out-remove-zero-time-clocks t)
  (org-startup-folded 'content)
  (org-columns-default-format "%50ITEM(Task) %5TODO(Todo) %10Effort(Effort){:} %2PRIORITY %TAGS")
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-span 'day)
  (org-agenda-log-mode-items (quote (closed clock)))
  (org-agenda-clockreport-parameter-plist
   '(:maxlevel 5 :block t :tstart t :tend t :emphasize t :link t :narrow 80 :indent t :formula nil
    :timestamp t :level 5 :tcolumns nil :formatter nil))
  (org-global-properties (quote ((
                                  "Effort_ALL" . "00:05 00:10 00:15 00:30 01:00 01:30 02:00 02:30 03:00"))))
  (org-agenda-files (quote (
                            "~/Private/task.org"
                            "~/Private/routine.org"
                            "~/Private/task.org_archive"
                            "~/Private/schedule.org")))
  :bind (("C-c C-c" . counsel-org-capture)
        ("M-o a" . org-agenda)
        ("C-x C-l" . org-store-link)
        :map org-mode-map
        ("C-c i" . org-clock-in)
        ("C-c o" . org-clock-out)
        ("C-c n" . org-narrow-to-subtree)
        ("C-c b" . org-narrow-to-block)
        ("C-c w" . widen)
        ("C-c e" . org-set-effort)))

(use-package org-bullets
    :hook (org-mode . org-bullets-mode))

;;;; HTML/CSS/SCSS/JS
(use-package scss-mode
  :mode "\\.scss\\'")

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))

;;;; Protobuf
(use-package protobuf-mode
  :mode "\\.proto$")

;;;; Fish
(use-package fish-mode
  :mode "\\.fish\\'")

;;;; Markdown
(use-package markdown-mode
  :custom
  (markdown-hide-markup nil)
  (markdown-bold-underscore t)
  (markdown-italic-underscore t)
  (markdown-header-scaling t)
  (markdown-indent-function t)
  (markdown-enable-math t)
  (markdown-hide-urls nil)
  :mode "\\.md\\'")

(use-package markdown-toc)

;;;; Log
(use-package logview :defer t)

;;;; Systemd
(use-package systemd
  :mode
  ("\\.service\\'" "\\.timer\\'" "\\.target\\'" "\\.mount\\'"
   "\\.automount\\'" "\\.slice\\'" "\\.socket\\'" "\\.path\\'"
   "\\.netdev\\'" "\\.network\\'" "\\.link\\'"))

;;;; YAML
(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))

;;;; Dockerfile
(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

;;;; JSON
(use-package json-mode
  :mode "\\.json\\'")

;;;;;
;;;;; Custom Function
;;;;;

;;;; Copy File PATH & NAME
(defun my/put-current-path-to-clipboard ()
    (interactive)
    (let ((file-path buffer-file-name)
          (dir-path default-directory))
      (cond (file-path
             (kill-new (expand-file-name file-path))
             (message "This file path is on the clipboard!"))
            (dir-path
             (kill-new (expand-file-name dir-path))
             (message "This directory path is on the clipboard!"))
            (t
             (error-message-string "Fail to get path name.")))))

  (defun my/put-current-filename-to-clipboard ()
    (interactive)
    (let ((file-path buffer-file-name)
          (dir-path default-directory))
      (cond (file-path
             (kill-new (file-name-nondirectory file-path))
             (message "This file path is on the clipboard!"))
            (dir-path
             (kill-new (file-name-nondirectory dir-path))
             (message "This directory path is on the clipboard!"))
            (t
             (error-message-string "Fail to get path name.")))))

;;;;;
;;;;; UI
;;;;;

;;;; Dashboard
 (use-package dashboard
  :diminish
  (dashboard-mode page-break-lines-mode)
  :custom
  (dashboard-startup-banner "~/.emacs.d/dashboard.txt")
  (dashboard-items '((recents . 15)
                     (projects . 5)
                     (bookmarks . 5)
                     (agenda . 5)))
  :hook
  (after-init . dashboard-setup-startup-hook)
  :config
  (add-to-list 'dashboard-items '(agenda) t))

;;;; Imenu list
(use-package imenu-list
  :bind
  ("<f10>" . imenu-list-smart-toggle)
  :custom-face
  (imenu-list-entry-face-1 ((t (:foreground "white"))))
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))

;;;; Transparency
(defun toggle-window-transparency ()
  "Cycle the frame transparency from default to transparent."
  (interactive)
  (let ((transparency 85)
        (opacity 100))
    (if (and (not (eq (frame-parameter nil 'alpha) nil))
             (< (frame-parameter nil 'alpha) opacity))
        (set-frame-parameter nil 'alpha opacity)
      (set-frame-parameter nil 'alpha transparency))))

(global-set-key (kbd "C-c C-t") 'toggle-window-transparency)

;;;; Neotree
(use-package neotree
  :commands
  (neotree-show neotree-hide neotree-dir neotree-find)
  :custom
  (neo-theme 'nerd2)
  :bind
  ("<f8>" . neotree-current-dir-toggle)
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
              (neotree-find file-name))))))
  (defun neotree-current-dir-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
             (ffip-project-root)
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

;;;; Doom Theme/ModeLine
(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :config
  (load-theme 'doom-nord t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
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
      '(misc-info github persp-name lsp debug minor-modes input-method buffer-encoding major-mode process vcs checker))))


;;;; Nyan Mode
(use-package nyan-mode
   :custom
   (nyan-cat-face-number 4)
   (nyan-animate-nyancat t)
   :hook
   (doom-modeline-mode . nyan-mode))

;;;; Hide Modeline
(use-package hide-mode-line
  :hook
  ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))

;;;; Fill Column indicator
(use-package fill-column-indicator
  :hook
  ((markdown-mode
    git-commit-mode) . fci-mode))

;;;;;
;;;;; HighLights
;;;;;

;;;; Highlight Line
(use-package hl-line
  :disabled
  :ensure nil
  :hook
  (after-init . global-hl-line-mode))

;;;; Paren
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

;;;; Highlight Symbol
(use-package highlight-symbol
  :bind
  (:map prog-mode-map
  ("M-o h" . highlight-symbol)
  ("M-o r" . highlight-symbol-remove-all)
  ("M-p" . highlight-symbol-prev)
  ("M-n" . highlight-symbol-next)))

;;;; Beacon
 (use-package beacon
  :custom
  (beacon-color "yellow")
  :config
  (beacon-mode 1))

;;;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;;; Rainbow Mode
(use-package rainbow-mode
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))

;;;; Volatile
(use-package volatile-highlights
  :diminish
  :hook
  (after-init . volatile-highlights-mode)
  :custom-face
  (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD")))))

;;;; Indent
(use-package highlight-indent-guides
  :diminish
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)) ; column

;;;;;
;;;;; Mark
;;;;;

;;;; Point Undo
(use-package point-undo
  :load-path "~/src/github.com/emacsmirror/point-undo"
  :bind
  ("C-," . point-undo)
  ("C-." . point-redo))

;;;; hydra setting
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(profiler-report)
(profiler-stop)
