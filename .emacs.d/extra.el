;; (ido-vertical-mode 1)

;; xclip
(require 'xclip)
(xclip-mode 1)

;;
(require 'buffer-move)

;; (global-set-key (kbd "M-x") 'smex)

;; buffers
(global-set-key (kbd "<C-M-up>")     'buf-move-up)
(global-set-key (kbd "<C-M-down>")   'buf-move-down)
(global-set-key (kbd "<C-M-left>")   'buf-move-left)
(global-set-key (kbd "<C-M-right>")  'buf-move-right)

;; manage files

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(z3-mode xclip use-package smex pythonic modern-cpp-font-lock markdown-mode magit ido-vertical-mode buffer-move boogie-friends)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'extra)
