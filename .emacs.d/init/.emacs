;; 
;; Emacs extensions
;; 

;; Environment defaults

(setq load-path (append load-path '("~/conf/.emacs.d/elisp")))
(setq backup-directory-alist `(("." . "~/.saves")))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(add-to-list
 'package-archives
 '("melpa-stable" . "https://stable.melpa.org/packages/")
 t)
(setq package-install-upgrade-built-in t)
(package-initialize)

;; External tools

(setq browse-url-browser-function                 ; firefox browser par défaut
      'browse-url-firefox)
(setq compile-command "make")

;; Edition defaults

(xterm-mouse-mode 1)
(global-set-key (kbd "<mouse-4>") 'previous-line)
(global-set-key (kbd "<mouse-5>") 'next-line)
(setq scroll-step 1
      auto-window-vscroll nil
      scroll-conservatively 10000) ; scroll one line by one line
(setq-default indent-tabs-mode nil) ; indentation: spaces
(delete-selection-mode t) ; hitting a letter overwrites the selection
(setq case-fold-search t) ; search is case-insensitive

;; IHM defaults

(menu-bar-mode -1) ; hide the menubar
(blink-cursor-mode 0) ; the cursor doesn't blink
(setq inhibit-startup-message t) ; no startup message
(setq initial-scratch-message nil) ; no initial scratch message

(show-paren-mode t) ; match the parenthesis
(global-hl-line-mode t) ; highlight the current line
(global-display-line-numbers-mode 1)  ; show the line numbers on the left
;; (setq-default word-wrap t)

(fset 'yes-or-no-p 'y-or-n-p) ; y-or-n replaces yes-or-no
(setq confirm-nonexistent-file-or-buffer nil ; create file without confirmation
      vc-follow-symlinks nil ; follows symlinks
      revert-without-query '(".*")) ; reverts buffer

(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-everywhere t
      ido-create-new-buffer 'always
      ido-auto-merge-work-directories-length -1)

(setq-default mode-line-buffer-identification
  (propertized-buffer-identification "%b"))
(defun shorten-directory (dir)
  (file-name-nondirectory (directory-file-name dir)))
(defvar mode-line-directory
  '(:propertize
    (:eval (shorten-directory default-directory)))
  "Formats the current directory.")
(put 'mode-line-directory 'risky-local-variable t)
(setq-default mode-line-format
       '(
         "%e"
         mode-line-front-space
         mode-line-directory ; current dir
         " "
         "%*" ; edited/read-only
         " "
         mode-line-buffer-identification ; the buffer's name
         "    "
         "%l,%c (%p)" ; the position in the buffer
         "    "
         "%m" ; major mode
         minor-mode-alist ; minor modes
         ))

;; (set-face-attribute 'tab-line nil ;; background behind tabs
;;                     :foreground (face-foreground 'default)
;;                     :background (face-background 'default)
;;                     :distant-foreground (face-background 'default)
;;                     :underline (face-foreground 'default)
;;                     :overline (face-background 'default)
;;                     :height (face-attribute 'default :height)
;;                     :box nil)
;; (set-face-attribute 'tab-line-tab-current nil
;;                     :inherit 'tab-line
;;                     :foreground (face-foreground 'default)
;;                     :background (face-background 'highlight)
;;                     :weight 'regular
;;                     :box nil)
;; (set-face-attribute 'tab-line-tab-modified nil
;;                     :inherit 'tab-line-tab-current
;;                     :foreground (face-foreground 'link)
;;                     :box nil)
;; (set-face-attribute 'tab-line-tab nil ;; active tab in another window
;;                     :inherit 'tab-line-current
;;                     :box nil)
;; (set-face-attribute 'tab-line-tab-inactive nil
;;                     :inherit 'tab-line
;;                     :box nil)
;; (set-face-attribute 'tab-line-highlight nil
;;                     :inherit 'tab-line-current
;;                     :foreground (face-foreground 'link)
;;                     :background 'unspecified
;;                     :box nil)

;; Extra packages

(require 'mlir-mode)
(require 'sail-mode)
(require 'extra-commands)
(require 'buffer-move)

;; Theme
(load-theme 'atom-one-dark t)

;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 50)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; xclip
(require 'xclip)
(xclip-mode 1)

;; Override (expand) existing behaviours

(advice-add 'comment-region :before #'copy-region-as-kill)
(global-set-key (kbd "C-w") 'kill-region-or-line)
(global-set-key (kbd "M-w") 'copy-region-or-line)
(global-set-key (kbd "C-a") 'smarter-beginning-of-line)
(global-set-key (kbd "M-x") 'smex)

;; Shortcuts

;; display informations
(global-set-key (kbd "C-x \"") 'display-fill-column-indicator-mode)
(global-set-key (kbd "M-l") 'count-lines-page)
(global-set-key (kbd "M-P") 'show-and-copy-file-name)
(global-set-key (kbd "M-p") 'show-file-name)

;; edit
(global-set-key (kbd "<M-down>")   'forward-paragraph)
(global-set-key (kbd "<M-up>")   'backward-paragraph)
(global-set-key (kbd "M-_") 'undo-only)
(global-set-key (kbd "C-x :") 'dabbrev-expand)

;; windows
(global-set-key (kbd "M-=") 'enlarge-window)
(global-set-key (kbd "M--") 'shrink-window)
(global-set-key (kbd "C-x N") 'other-window)
(global-set-key (kbd "C-x P") 'myprevious-window)
(global-set-key (kbd "<C-right>")   'windmove-right)
(global-set-key (kbd "<C-left>")   'windmove-left)
(global-set-key (kbd "<C-up>")   'windmove-up)
(global-set-key (kbd "<C-down>")   'windmove-down)
(global-set-key (kbd "<C-M-right>")   'tab-line-switch-to-next-tab)
(global-set-key (kbd "<C-M-left>")   'tab-line-switch-to-prev-tab)
(global-set-key (kbd "C-M-q")   'bury-buffer)
(global-unset-key (kbd "C-M-t"))
(global-set-key (kbd "C-M-t")   'global-tab-line-mode)

;; buffers
(global-set-key (kbd "C-x n") 'switch-to-next-buffer)
(global-set-key (kbd "C-x p") 'switch-to-prev-buffer)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; manage files
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
;; (global-set-key (kbd "C-x C-b") 'electric-buffer-list)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "<f5>") 'revert-buffer)

;; compile
(global-set-key (kbd "C-x t") 'eshell)
(global-set-key (kbd "C-x D") 'compile)
(global-set-key (kbd "C-x d") 'recompile)
(global-set-key (kbd "<f1>") 'compile)
(global-set-key (kbd "C-<f1>") 'new-compile-cmd)
(global-set-key (kbd "<f2>") 'recompile)
(global-set-key (kbd "<f3>") 'next-error)

;; command

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit tuareg xclip use-package smex pdf-tools modern-cpp-font-lock markdown-mode ido-vertical-mode buffer-move atom-one-dark-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

