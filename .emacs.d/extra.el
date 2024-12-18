;; Elisp primitives

(use-package seq
  :ensure t)

;; File & buffer management

(use-package recentf
  :ensure t
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 50))

(use-package consult
  :ensure t
  :bind ("C-x C-r" . consult-recent-file))

(use-package ibuffer
  :ensure t
  :bind ("C-x B" . ibuffer)
  :config
  (setq ibuffer-expert t))

;; Window management

;; C-x w <n>: go to window n (kill if negative)
(use-package winum
  :ensure t
  :config
  (winum-mode))

(use-package windresize
  :ensure t
  :bind
  ("C-c w" . windresize))

(use-package avy
  :ensure t
  :bind
  ("M-g c" . avy-goto-char)
  ("M-g l" . avy-goto-line)
  ("M-g w" . avy-goto-word-1))

;; System

(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

;; Doc

;; Show descriptions in fido
;; (use-package marginalia
;;   :ensure t
;;   :config
;;   (marginalia-mode))

(use-package free-keys
  :ensure t
  :commands free-keys)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Emacs-style edition

(use-package move-text
  :ensure t
  :bind (("C-M-<up>" . move-text-up)
         ("C-M-<down>" . move-text-down)))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode +1)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

(provide 'extra)
