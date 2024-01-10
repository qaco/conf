;; 
;; Emacs extensions
;; 

(package-initialize)

(setq load-path (append load-path '("~/conf/.emacs.d/elisp")))
;; (setq load-path (append load-path '("~/.emacs.d/elisp")))

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)

(add-to-list
 'package-archives
 '("melpa-stable" . "https://stable.melpa.org/packages/")
 t)

;; (unless package-archive-contents
;;   (package-refresh-contents))
;; (setq package-list '(spacemacs-theme
;; 		     buffer-move
;; 		     ido-vertical-mode
;;                      smex
;;                      magit
;;                      recentf))
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))

;;
;; Minor modes
;;

(require 'xclip)
(xclip-mode 1)

;; ido
(ido-mode 1)                                      ; active ido
(ido-vertical-mode 1)                             ; disposition verticale
(setq ido-everywhere t                            ; tous les buffers/fichiers
      ido-create-new-buffer 'always               ; nveau quand pas trouvé
      ido-auto-merge-work-directories-length -1)  ; pas ds les autres dossiers

;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 25)                 ; 50 derniers fichiers
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; Mode-line

(setq-default mode-line-buffer-identification
  (propertized-buffer-identification "%b"))
(defun shorten-directory (dir)
  (file-name-nondirectory
   (directory-file-name dir)))
(defvar mode-line-directory
  '(:propertize
    (:eval (shorten-directory default-directory)))
  "Formats the current directory.")
(put 'mode-line-directory 'risky-local-variable t)
(setq-default mode-line-format
       '(
         "%e"
         mode-line-front-space
         mode-line-directory                      ; dossier courant
         " "
         "%*"                                     ; modifié/read-only
         " "
         mode-line-buffer-identification          ; le nom du buffer
         "    "
         "%l,%c (%p)"                       ; position
         "    "
         "%m"                                     ; major mode
         minor-mode-alist                         ; minor modes
         ))

;; 
;; Emacs appearence
;; 

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'atom-one-dark t)

;; Startup
(add-to-list 'default-frame-alist '(height . 30)) ; hauteur par défaut
(add-to-list 'default-frame-alist '(width . 81))  ; largeur par défaut
(setq inhibit-startup-message t)                  ; cacher startup-message
(setq initial-scratch-message nil)

;; Window
(setq frame-title-format "%b"                     ; titre : nom du fichier
      icon-title-format "%b")                     ; titre : nom du fichier
(menu-bar-mode -1)                                ; cacher barre de menu
(tool-bar-mode -1)                                ; cacher outils
(scroll-bar-mode -1)                              ; cacher scroll bar
(blink-cursor-mode 0)                             ; curseur fixe

;; Buffer
(setq-default indicate-empty-lines t)             ; marque lignes vides
(show-paren-mode t)                               ; matching des parentheses
(global-hl-line-mode t)                           ; highlight ligne courante

;; Line/column
(setq-default fill-column 79                  ; largeur de page : 80 char
              word-wrap t)                    ; coupe après le mot
(global-visual-line-mode t)
(column-number-mode 1)                            ; afficher numéro de colonne
(line-number-mode 1)                              ; afficher numéro de ligne
(size-indication-mode 1)

;; Control

;; External tools
(setq browse-url-browser-function                 ; firefox browser par défaut
      'browse-url-firefox)
(setq compile-command "make")

;; Default behaviours
(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil)
(setq-default indent-tabs-mode nil)
(delete-selection-mode t)                     ; overwrite region
(setq case-fold-search t)                     ; search ignore la casse
(setq backup-directory-alist `(("." . "~/.saves")))

;; No questions
(fset 'yes-or-no-p 'y-or-n-p)                 ; y-or-n remplace yes-or-no
(setq confirm-nonexistent-file-or-buffer nil  ; créa file sans confirmer
      vc-follow-symlinks nil                  ; symlink sans confirmer
      revert-without-query '(".*"))           ; revert sans confirmer

;; Advices
(advice-add 'comment-region :before #'copy-region-as-kill)
(advice-add 'split-window-horizontally :after #'balance-windows)

;;
;; Major modes
;;

(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)
(require 'mlir-mode)

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; Shortcuts

(require 'extra-commands)
(require 'buffer-move)

;; display informations
(global-set-key (kbd "C-x \"") 'display-fill-column-indicator-mode)
(global-set-key (kbd "C-x l") 'display-line-numbers-mode)
(global-set-key (kbd "M-l") 'count-lines-page)

;; edit
(global-set-key (kbd "C-a") 'smarter-beginning-of-line)
(global-set-key (kbd "C-<return>") 'newline-above)
(global-set-key (kbd "C-w") 'kill-region-or-line)
(global-set-key (kbd "M-w") 'copy-region-or-line)
(global-set-key (kbd "M-_") 'undo-only)
(global-set-key (kbd "C-:") 'dabbrev-expand)

;; manage windows
(global-set-key (kbd "C-x <") 'enlarge-window)
(global-set-key (kbd "C-x >") 'shrink-window)
(global-set-key (kbd "C-x n") 'switch-to-next-buffer)
(global-set-key (kbd "C-x p") 'switch-to-prev-buffer)
(windmove-default-keybindings) ; S-arrow pour naviger
(global-set-key (kbd "C-x N") 'other-window)
(global-set-key (kbd "C-x P") 'myprevious-window)
;; (global-set-key (kbd "C-x P") 'windmove-left)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)
;; (global-set-key (kbd "<f5>") 'split-window-horizontally)
;; (global-set-key (kbd "<f6>") 'split-window-vertically)

;; manage files
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(global-set-key (kbd "C-x g") 'magit-status)
 (global-set-key (kbd "<f4>") 'revert-buffer)

;; compile
(global-set-key (kbd "C-x t") 'term)
(global-set-key (kbd "C-x D") 'compile)
(global-set-key (kbd "C-x d") 'recompile)
(global-set-key (kbd "<f1>") 'compile)
(global-set-key (kbd "C-<f1>") 'new-compile-cmd)
(global-set-key (kbd "<f2>") 'recompile)
(global-set-key (kbd "<f3>") 'next-error)

;; command
(global-set-key (kbd "M-x") 'smex)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(elpy xclip use-package spacemacs-theme spacegray-theme smex seti-theme pdf-tools nord-theme modern-cpp-font-lock markdown-mode magit ido-vertical-mode gruvbox-theme buffer-move base16-theme atom-one-dark-theme afternoon-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
