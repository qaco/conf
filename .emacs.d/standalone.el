;; External tools

(defvar browse-url-browser-function 'browse-url-firefox)
(defvar compile-command "make")

;; Edition defaults

(global-auto-revert-mode t)

;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 50)

;; Mouse in the therminal
(xterm-mouse-mode 1)
(global-set-key (kbd "<mouse-4>") 'previous-line)
(global-set-key (kbd "<mouse-5>") 'next-line)

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
(global-display-line-numbers-mode 1)  ; show line numbers
;; (setq-default word-wrap t)

;; Disable line numbers in term and eshell modes
(defun my-term-mode-setup ()
  "Custom configuration for `term-mode`."
  (display-line-numbers-mode -1))
(add-hook 'term-mode-hook 'my-term-mode-setup)
(add-hook 'eshell-mode-hook 'my-term-mode-setup)

;; Less confirms
(fset 'yes-or-no-p 'y-or-n-p) ; use y/n instead of yes/no
(setq confirm-nonexistent-file-or-buffer nil ; create file without confirm
      vc-follow-symlinks nil ; follows symlinks
      revert-without-query '(".*")) ; reverts buffer

;; Ido bheaviour
(ido-mode 1)
(ido-vertical-mode 1)
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
         minor-mode-alist ; minor modes
         ))

;; Mode hooks

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

;; display informations
(global-set-key (kbd "C-x \"") 'display-fill-column-indicator-mode)
(global-set-key (kbd "M-l") 'count-lines-page)
(global-set-key (kbd "M-I") 'show-and-copy-file-name)
(global-set-key (kbd "M-i") #'(lambda() (interactive) (message (buffer-file-name))))
(global-set-key (kbd "M-I") #'(lambda() (interactive) (kill-new (buffer-file-name))))

;; windows
(global-set-key (kbd "M-=") 'enlarge-window)
(global-set-key (kbd "M--") 'shrink-window)
(global-set-key (kbd "C-x N") 'other-window)
(global-set-key (kbd "C-x P") #'(lambda() (interactive) (other-window -1)))
(global-set-key (kbd "<C-right>")   'windmove-right)
(global-set-key (kbd "<C-left>")   'windmove-left)
(global-set-key (kbd "<C-up>")   'windmove-up)
(global-set-key (kbd "<C-down>")   'windmove-down)

;; buffers
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x n") 'switch-to-next-buffer)
(global-set-key (kbd "C-x p") 'switch-to-prev-buffer)

;; edit
(advice-add 'comment-region :before #'copy-region-as-kill)
(global-set-key (kbd "C-q")   'backward-delete-char-untabify)
(global-set-key (kbd "M-_") 'undo-only)
(global-set-key (kbd "C-x :") 'dabbrev-expand)
(global-set-key (kbd "C-x q") 'join-line)
(global-set-key (kbd "C-w") 'kill-region-or-line)
(global-set-key (kbd "M-w") 'copy-region-or-line)
(global-set-key (kbd "C-a") 'smarter-beginning-of-line)

;; navigation
(global-set-key (kbd "<mouse-4>") 'previous-line)
(global-set-key (kbd "<mouse-5>") 'next-line)
(global-set-key (kbd "M-<mouse-4>") #'(lambda() (interactive) (forward-line -5)))
(global-set-key (kbd "M-<mouse-5>") #'(lambda() (interactive) (forward-line 5)))
(global-set-key (kbd "<M-down>") #'(lambda() (interactive) (forward-line 5)))
(global-set-key (kbd "<M-up>") #'(lambda() (interactive) (forward-line -5)))
(global-set-key (kbd "M-n") #'(lambda() (interactive) (forward-line 5)))
(global-set-key (kbd "M-p") #'(lambda() (interactive) (forward-line -5)))

(global-set-key (kbd "C-x t") 'term)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(defun smarter-beginning-of-line (arg)
  
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  
  (interactive "^p")
  
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun wise-kill-whitespaces ()
 (if (< (line-end-position) (point-max))
	  (delete-region (point) (+ 1 (line-end-position)))
	(delete-region (point) (line-end-position))))

(defun wise-copy-line (should-kill)

  "Kills the current line preserving column position. Doesn't save nor newline
char nor indentation (doesn't save anything if blank line.)"
  
  (interactive)

  ;; Save the column position and go to indentation
  (let ((former-column (current-column)))
    (move-to-column (current-indentation))
    ;; Copy & delete (instead of kill) the text until the end of line
    (call-interactively 'set-mark-command)
    (call-interactively 'end-of-line)
    (copy-region-as-kill (mark) (point))
    (when should-kill
      (delete-region (mark) (point))
      ;; Remove all remaining whitespaces (including newline)
      (beginning-of-line)
      (wise-kill-whitespaces))
    ;; Restore the column position
    (move-to-column former-column)))

(defun kill-region-or-line ()

  "Kills marked region if exists, current line otherwise."
  
  (interactive)

  (if mark-active
      (kill-region (mark) (point))
    (wise-copy-line 1)))

(defun copy-region-or-line ()

  "Saves marked region if exists, current line otherwise."
  
  (interactive)

  (if mark-active
      (copy-region-as-kill (mark) (point))
    (wise-copy-line nil)))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(provide 'standalone)
