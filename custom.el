(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-migemo-function-names
   '(swiper--add-overlays-migemo
     (swiper--re-builder :around swiper--re-builder-migemo-around)
     (ivy--regex :around ivy--regex-migemo-around)
     (ivy--regex-ignore-order :around ivy--regex-ignore-order-migemo-around)
     (ivy--regex-plus :around ivy--regex-plus-migemo-around)
     ivy--highlight-default-migemo ivy-occur-revert-buffer-migemo ivy-occur-press-migemo avy-migemo-goto-char avy-migemo-goto-char-2 avy-migemo-goto-char-in-line avy-migemo-goto-char-timer avy-migemo-goto-subword-1 avy-migemo-goto-word-1 avy-migemo-isearch avy-migemo-org-goto-heading-timer avy-migemo--overlay-at avy-migemo--overlay-at-full))
 '(lsp-log-io nil nil nil "Customized with use-package lsp-mode")
 '(org-agenda-files nil nil nil "Customized with use-package org")
 '(package-selected-packages
   '(org nord-theme python-mode go-tag gotest flx counsel-projectile fill-column-indicator hungry-delete multiple-cursors fish-mode flymake blacken blackboard-bold-mode poetry py-isort pyenv-mode lsp-java dap-mode lsp-ui dockerfile-mode go-mode yaml-mode lsp-metals volatile-highlights highlight-symbol dimmer forge browse-at-remote diffview yasnippet persp-mode ace-window rotate gruvbox-theme nyan-mode avy-migemo ivy-migemo anzu mwim wgrep-pt list-packages-ext popwin shut-up evil-smartparens smartparens projectile all-the-icons-ivy-rich rg sequential-command-config-version imenu-list undo-tree exec-path-from-shell ag fzf all-the-icons-ivy ivy-pass counsel ivy which-key ivy-hydra sequential-command rainbow-mode sequential-command-config hydra git-gutter magit dashboard ivy-rich beacon highlight-indent-guides rainbow-delimiters hide-mode-line org-bullets all-the-icons doom-modeline neotree doom-themes swiper ## use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
 '(doom-modeline-bar ((t (:background "#6272a4"))))
 '(git-gutter:added ((t (:background "#50fa7b"))))
 '(git-gutter:deleted ((t (:background "#ff79c6"))))
 '(git-gutter:modified ((t (:background "#f1fa8c"))))
 '(imenu-list-entry-face-1 ((t (:foreground "white"))))
 '(show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))))
 '(vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD")))))
