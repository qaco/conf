;; display informations
(global-set-key (kbd "C-x \"") 'display-fill-column-indicator-mode)
(global-set-key (kbd "C-x l") 'count-lines-page)

;; windows
(global-set-key (kbd "C-x 2") 'split-window-below-and-center-cursor)
(global-set-key (kbd "M-=") 'enlarge-window)
(global-set-key (kbd "M--") 'shrink-window)
(global-set-key (kbd "C-x N") 'other-window)
(global-set-key (kbd "C-x P") #'(lambda() (interactive) (other-window -1)))
(global-set-key (kbd "<C-right>")   'windmove-right)
(global-set-key (kbd "<C-left>")   'windmove-left)
(global-set-key (kbd "<C-up>")   'windmove-up)
(global-set-key (kbd "<C-down>")   'windmove-down)

;; edit
(advice-add 'comment-region :before #'copy-region-as-kill)
(global-set-key (kbd "C-q")   'delete-region)
(global-set-key (kbd "C-j") #'(lambda() (interactive) (delete-region (point) (line-end-position))))
(global-set-key (kbd "M-_") 'undo-only)
(global-set-key (kbd "C-x :") 'dabbrev-expand)
(global-set-key (kbd "C-x q") 'join-line)
(global-set-key (kbd "C-w") #'(lambda() (interactive) (if mark-active
                                                          (kill-region (mark) (point))
                                                        (wise-kill-line))))
(global-set-key (kbd "M-w") #'(lambda() (interactive) (if mark-active
                                                          (copy-region-as-kill (mark) (point))
                                                        (wise-copy-line))))

;; navigation
(global-set-key (kbd "C-a") 'smarter-beginning-of-line)
(global-set-key (kbd "<mouse-4>") 'previous-line)
(global-set-key (kbd "<mouse-5>") 'next-line)
(global-set-key (kbd "M-<mouse-4>") #'(lambda() (interactive) (forward-line -5)))
(global-set-key (kbd "M-<mouse-5>") #'(lambda() (interactive) (forward-line 5)))
(global-set-key (kbd "<M-down>") #'(lambda() (interactive) (forward-line 5)))
(global-set-key (kbd "<M-up>") #'(lambda() (interactive) (forward-line -5)))
(global-set-key (kbd "M-n") #'(lambda() (interactive) (forward-line 5)))
(global-set-key (kbd "M-p") #'(lambda() (interactive) (forward-line -5)))

;; buffers

(global-set-key (kbd "C-x C-<up>")     #'(lambda() (interactive) (swap-buffer-with-adjacent 'above)))
(global-set-key (kbd "C-x C-<down>")   #'(lambda() (interactive) (swap-buffer-with-adjacent 'below)))
(global-set-key (kbd "C-x C-<left>")   #'(lambda() (interactive) (swap-buffer-with-adjacent 'left)))
(global-set-key (kbd "C-x C-<right>")  #'(lambda() (interactive) (swap-buffer-with-adjacent 'right)))
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

;; Git

(global-set-key (kbd "C-x v A") 'my-git-add-update)
(global-set-key (kbd "C-x v c") 'my-git-commit)
(global-set-key (kbd "C-x v C") 'my-git-amend)
(global-set-key (kbd "C-x v p") 'my-git-push)

;; System

(global-set-key (kbd "C-x C-w") 'write-file)
(global-set-key (kbd "C-x w") 'save-buffer-copy)
(global-set-key (kbd "C-x t") 'term)
(global-set-key (kbd "M-o i") #'(lambda() (interactive) (message (buffer-file-name))))
(global-set-key (kbd "M-o M-i") #'(lambda() (interactive) (kill-new (message (buffer-file-name)))))

;; External tools

(defvar browse-url-browser-function 'browse-url-firefox)
(defvar compile-command "make")

;; Edition defaults

(global-auto-revert-mode t)
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)

;; Mouse in the therminal
(xterm-mouse-mode 1)

;; Scroll one line at a time
(setq scroll-step 1
      auto-window-vscroll nil
      scroll-conservatively 10000)

(setq-default indent-tabs-mode nil) ; uses spaces for indentation
(delete-selection-mode t) ; typing replaces the selection
(setq case-fold-search t) ; case-insensitive search

;; User inteface defaults

(menu-bar-mode -1) ; hide the menubar
(blink-cursor-mode 0) ; stop the cursor from blinking
(setq inhibit-startup-message t) ; disable startup message
(setq initial-scratch-message nil) ; disable initial scratch message

(show-paren-mode t) ; highlight matching parentheses
(global-hl-line-mode t) ; highlight the current line

;; Less confirms
(fset 'yes-or-no-p 'y-or-n-p) ; use y/n instead of yes/no
(setq confirm-nonexistent-file-or-buffer nil ; create file without confirm
      vc-follow-symlinks nil ; follows symlinks
      revert-without-query '(".*")) ; reverts buffer

(icomplete-mode 1)
(setq read-file-name-completion-ignore-case t)
(setq read-file-name-function 'read-file-name-default)

(fido-vertical-mode 1)
(setq ido-everywhere t
      ido-create-new-buffer 'always
      ido-auto-merge-work-directories-length -1)

;; Mode-line (the global mode displaying the info line of each buffer)

(setq-default mode-line-buffer-identification
  (propertized-buffer-identification "%b"))
(defun shorten-directory (dir)
  "Return the last part of the directory path."
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
         mode-line-directory ; current directory
         " "
         "%*" ; edited/read-only
         " "
         mode-line-buffer-identification ; current buffer
         "    "
         "%l,%c (%p)" ; position in buffer
         "    "
         "%m" ; major mode
         ;; minor-mode-alist ; minor modes
         ))

;; Hooks

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; term and eshell modes hooks
(defun my-term-mode-setup ()
  "Custom configuration for `term-mode`."
  (display-line-numbers-mode -1))
(add-hook 'term-mode-hook 'my-term-mode-setup)
(add-hook 'eshell-mode-hook 'my-term-mode-setup)

;; text mode hooks
(add-hook 'text-mode-hook 'visual-line-mode)

;; Theme

(load-theme 'modus-vivendi t)

(provide 'standalone)
