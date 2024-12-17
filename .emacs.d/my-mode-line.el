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

(provide 'my-mode-line)
