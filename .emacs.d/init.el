;; Packages

(setq custom-file null-device)

(add-to-list
 'package-archives
 '("melpa-stable" . "https://stable.melpa.org/packages/")
 t)
;; (setq package-install-upgrade-built-in t)
(package-initialize)

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(setq package-archive-enable-alist nil)

;; Environment variables

(setq load-path (append load-path '("~/conf/.emacs.d/elisp")))
(setq custom-file "~/conf/.emacs.d/extra.el")
(setq load-path (append load-path '("~/conf/.emacs.d")))
(setq backup-directory-alist `(("." . "~/.saves")))

(require 'standalone)
(require 'my-mode-line)
(require 'mlir-mode)
(require 'tablegen-mode)
(require 'git)
(require 'org-conf)
(require 'console)
(require 'ide)
(require 'extra)
