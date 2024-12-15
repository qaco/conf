(setq package-install-upgrade-built-in t)

;; Elisp primitives

(use-package company
  :ensure t)

;; Emacs global

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

;; Edition

(use-package avy
  :ensure t
  :bind
  ("M-g c" . avy-goto-char)
  ("M-g l" . avy-goto-line)
  ("M-g w" . avy-goto-word-1))

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

(use-package git-timemachine
  :ensure t
  :bind ("C-x v t" . git-timemachine))

(use-package git-gutter
  :ensure t
  :hook ((prog-mode . git-gutter-mode)
         (text-mode . git-gutter-mode))
  :config
  (set-face-background 'git-gutter:modified "purple")
  (set-face-foreground 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:deleted "red")
  :bind ("C-x v a" . 'git-gutter:stage-hunk))

(global-set-key (kbd "C-x v A") 'my-git-add-update)
(global-set-key (kbd "C-x v c") 'my-git-commit)
(global-set-key (kbd "C-x v C") 'my-git-amend)
(global-set-key (kbd "C-x v p") 'my-git-push)

(defun my-git-add-update()
  "Run 'git add -u' in the current directory. Display the list of
   files to be staged and ask for confirmation before proceeding."
  (interactive)
  (let* ((default-directory (if (buffer-file-name)
                                (file-name-directory (buffer-file-name))
                              default-directory))
         (staged-files (shell-command-to-string "git diff --name-only --cached"))
         (unstaged-files (shell-command-to-string "git diff --name-only")))
    (if (string-empty-p unstaged-files)
        (message "No changes to stage.")
      (if (yes-or-no-p (format "The following files will be staged:\n%s\nProceed? " unstaged-files))
          (progn
            (shell-command "git add -u")
            (message "Files staged:\n%s" (shell-command-to-string "git diff --name-only --cached")))
        (message "Operation canceled.")))))

(defun my-git-commit ()
  "Run 'git commit' in the current directory. Prompt for a commit
    message in the minibuffer. If the commit fails, display the error
    message in the minibuffer."
  (interactive)
  (let ((commit-message (read-string "Commit message: ")))
    (with-temp-buffer
      (let ((exit-code
             (call-process-shell-command
              (concat "git commit -m " (shell-quote-argument commit-message))
              nil t)))
        (if (eq exit-code 0)
            (message "Commit successful!")
          (let ((error-message (buffer-string)))
            (message "Commit failed: %s" error-message)))))))

(defun my-git-amend ()
  "Run 'git commit --amend --no-edit' in the current directory. If
   the commit fails, display the error message in the minibuffer."
  (interactive)
  (with-temp-buffer
    (let ((exit-code
           (call-process-shell-command "git commit --amend --no-edit" nil t)))
      (if (eq exit-code 0)
          (message "Amend successful!")
        (let ((error-message (buffer-string)))
          (message "Amend failed: %s" error-message))))))

(defun my-git-push ()
  "Run 'git push origin' in the current directory. Prompt for the
   branch to push in the minibuffer. The default branch is 'main'
   or 'master' based on what is present locally. If the push fails,
   display the error message in the minibuffer."
  (interactive)
  (let* ((default-branch
           (if (zerop (call-process-shell-command "git rev-parse --verify main" nil nil))
               "main"
             (if (zerop (call-process-shell-command "git rev-parse --verify master" nil nil))
                 "master"
               (error "Neither 'main' nor 'master' branch exists"))))
         (branch (read-string (format "Branch to push (default %s): " default-branch) nil nil default-branch)))
    (with-temp-buffer
      (let ((exit-code
             (call-process-shell-command
              (concat "git push origin " branch)
              nil t)))
        (if (eq exit-code 0)
            (message "Push successful!")
          (let ((error-message (buffer-string)))
            (message "Push failed: %s" error-message)))))))

;; Displays the git blame on the current line
;; (use-package blamer
;;   :ensure t
;;   :config
;;   (global-blamer-mode 1))

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

;; Maybe redundant with eglot ?
;; M-. is 'go to definition'
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
 '(package-selected-packages '(z3-mode use-package boogie-friends)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'extra)
