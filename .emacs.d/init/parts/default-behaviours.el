;; external tools

(setq browse-url-browser-function                 ; firefox browser par défaut
      'browse-url-firefox)
(setq compile-command "make")

;; scroll doesn't jump

(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil)

;; prompt-free

(fset 'yes-or-no-p 'y-or-n-p)                 ; y-or-n remplace yes-or-no
(setq confirm-nonexistent-file-or-buffer nil  ; créa file sans confirmer
      vc-follow-symlinks nil                  ; symlink sans confirmer
      revert-without-query '(".*"))           ; revert sans confirmer

;; format

(setq-default fill-column 79                  ; largeur de page : 80 char
              word-wrap t)                    ; coupe après le mot

(global-visual-line-mode t)
(setq indent-tabs-mode nil)

;; edition/navigation

(delete-selection-mode t)                     ; overwrite region
(setq case-fold-search t)                     ; search ignore la casse

;;

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; advices

(advice-add 'comment-region :before #'copy-region-as-kill)
(advice-add 'split-window-horizontally :after #'balance-windows)

;; copy & paste

(require 'xclip)
(xclip-mode 1)

;; com protocols

;; (require 'term-keys)
;; (term-keys-mode t)

;; (use-package kkp
;;   :ensure t
;;   :config
;;   (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
;;   (global-kkp-mode +1))

(provide 'default-behaviours)
