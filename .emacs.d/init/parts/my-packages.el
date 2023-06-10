(require 'package)

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)

(add-to-list
 'package-archives
 '("melpa-stable" . "https://stable.melpa.org/packages/")
 t)

(setq package-list '(spacemacs-theme
		     buffer-move
		     ido-vertical-mode
                     smex
                     magit
                     recentf))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'my-packages)
