(setq-default mode-line-buffer-identification
  (propertized-buffer-identification "%b"))

(defun shorten-directory (dir)
  "Return the last part of the directory path."
  (file-name-nondirectory (directory-file-name dir)))

(defun get-git-branch ()
  "Return the current Git branch if the buffer is in a Git repository."
  (when (and buffer-file-name
             (locate-dominating-file buffer-file-name ".git"))
    (let ((branch (string-trim
                   (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
      (if (string-empty-p branch)
          nil
        branch))))

(defvar mode-line-directory
  '(:propertize
    (:eval (shorten-directory default-directory)))
  "Formats the current directory.")
(put 'mode-line-directory 'risky-local-variable t)

(defvar mode-line-git-branch
  '(:eval (when-let ((branch (get-git-branch)))
            (format " (%s)" branch)))
  "Displays the current Git branch in the mode line.")
(put 'mode-line-git-branch 'risky-local-variable t)

(setq-default mode-line-format
              '("%e"
                mode-line-directory ; current directory
                mode-line-git-branch ; current Git branch
                "  //  "
                "%*" ; edited/read-only
                " "
                mode-line-buffer-identification ; current buffer
                "  //  "
                "%l,%c -- %p" ; position in buffer
                "  //  "
                "%m" ; major mode
                ;; minor-mode-alist ; minor modes
                ))

(provide 'my-mode-line)
