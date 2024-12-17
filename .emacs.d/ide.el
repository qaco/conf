(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-workspace-sync t)
  (setq lsp-auto-configure-environment 'force)
  (lsp-enable-which-key-integration t)
  (add-hook 'python-mode-hook 'lsp)
  (add-hook 'lsp-workspace-sync-hook 'lsp-workspace-sync-save)
  (add-hook 'lsp-workspace-sync-hook 'lsp-workspace-sync-open)
  (add-hook 'lsp-workspace-sync-hook 'lsp-workspace-sync-delete)
  (add-hook 'lsp-workspace-sync-hook 'lsp-workspace-sync-rename)
  (add-hook 'lsp-workspace-sync-hook 'lsp-workspace-sync-move)
  (add-hook 'lsp-workspace-sync-hook 'lsp-workspace-sync-add)
  )

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

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  ;; :config
  ;; (setq lsp-ui-sideline-enable t
  ;;       lsp-ui-sideline-show-hover t)
  )

(use-package highlight-indentation
  :ensure t
  :config
  (setq highlight-indentation-blank-lines t)
  :hook (prog-mode . highlight-indentation-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (z3-smt2-mode . rainbow-delimiters-mode)
         (z3-mode . rainbow-delimiters-mode)))

;; (use-package smartparens
;;   :ensure t
;;   :config
;;   (smartparens-global-mode t))

;; (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

;; buggy
;; (defun my/pyright-buffer ()
;;   (interactive)
;;   (let* ((project (project-current t))
;;          (file (completing-read "Select file: " (project-files project)))
;;          (buffer-name (format "*pyright-%s*" (file-name-nondirectory file)))
;;          (buffer (get-buffer-create buffer-name)))
;;     (with-current-buffer buffer
;;       (read-only-mode -1)
;;       (insert (shell-command-to-string (concat "pyright " file)))
;;       (read-only-mode 1))
;;     (switch-to-buffer buffer)
;;     (file-notify-add-watch file
;;                            '(change)
;;                            (lambda (event)
;;                              (let ((file (cadr event)))
;;                                (with-current-buffer buffer
;;                                  (read-only-mode -1)
;;                                  (erase-buffer)
;;                                  (insert (shell-command-to-string (concat "pyright " file)))
;;                                  (read-only-mode 1)))))))

(global-set-key (kbd "C-c p r") 'my/pyright-buffer)

(provide 'ide)
