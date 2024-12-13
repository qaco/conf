(setq package-install-upgrade-built-in t)

;; Elisp primitives

(use-package company
  :ensure t)

;; Emacs global

;; C-x w <n>: go to window n (kill if negative)
(use-package winum
  :ensure t
  :config
  (winum-mode))

;; Show descriptions in fido
;; (use-package marginalia
;;   :ensure t
;;   :config
;;   (marginalia-mode))

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package free-keys
  :ensure t
  :commands free-keys)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (setq dashboard-center-content t)
;;   (dashboard-setup-startup-hook))

;; (use-package persistent-scratch
;;   :ensure t
;;   :config
;;   (persistent-scratch-setup-default))

;; Edition

(use-package move-text
  :ensure t
  :bind (("C-M-<up>" . move-text-up)
         ("C-M-<down>" . move-text-down)))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode +1)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

;; Git

(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter
  :ensure t
  :hook ((prog-mode . git-gutter-mode)
         (text-mode . git-gutter-mode))
  :config
  (set-face-background 'git-gutter:modified "purple")
  (set-face-foreground 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:deleted "red"))

(use-package git-timemachine
  :ensure t
  :bind ("C-x v t" . git-timemachine))

;; Emacs as an IDE

(use-package highlight-indentation
  :ensure t
  :config
  (setq highlight-indentation-blank-lines t)
  :hook (prog-mode . highlight-indentation-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (z3-mode . rainbow-delimiters-mode)))

(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-auto-configure-environment 'force)
  (lsp-enable-which-key-integration t)
  (add-hook 'python-mode-hook 'lsp))

(use-package company
  :ensure t)

;; The installation of pyright system-wide (using npm) is required
;; Derived from https://blog.serghei.pl/posts/emacs-python-ide/
(use-package lsp-pyright
  :ensure t
  :custom
  (lsp-pyright-langserver-command "pyright")
  :hook
  (python-mode . (lambda () (require 'lsp-pyright) (lsp))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(z3-mode use-package boogie-friends)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'extra)
