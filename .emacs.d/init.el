;; Packages

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

;; Environment variables

(setq load-path (append load-path '("~/conf/.emacs.d/elisp")))
(setq custom-file "~/conf/.emacs.d/extra.el")
(setq load-path (append load-path '("~/conf/.emacs.d")))
(setq backup-directory-alist `(("." . "~/.saves")))

(require 'standalone)
(require 'extra)

(require 'mlir-mode)
