(defun swap-buffer-with-adjacent (direction)
  "Swap the current buffer with the buffer in the adjacent window in the specified DIRECTION,
and move the cursor to the adjacent window."
  (let ((current-window (selected-window))
        (other-window (window-in-direction direction)))
    (when other-window
      (let ((current-buffer (window-buffer current-window))
            (other-buffer (window-buffer other-window)))
        (set-window-buffer current-window other-buffer)
        (set-window-buffer other-window current-buffer)
        (select-window other-window)))))

(defun split-window-below-and-center-cursor ()
  "Split the window horizontally and center the cursor both in the old window and in the new one."
  (interactive)
  (let ((current-window (selected-window)))
    (select-window (split-window-below))
    (recenter-top-bottom)
    (select-window current-window)
    (recenter-top-bottom)))

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

(defun wise-copy-line ()
  (interactive)
  (wise-kill-or-copy-line nil))

(defun wise-kill-line ()
  (interactive)
  (wise-kill-or-copy-line 1))

(defun wise-kill-or-copy-line (should-kill)

  "Copy/kill the current line preserving column position. Doesn't save nor newline
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

(defun save-buffer-copy (filename)
  "Save a copy of the current buffer to a specified FILENAME without changing the buffer."
  (interactive "FSave buffer copy to file: ")
  (write-region (point-min) (point-max) filename)
  (message "Buffer saved to %s" filename))

(defun my-git-get-repo-root ()
  "Return the root directory of the current Git repository."
  (string-trim (shell-command-to-string "git rev-parse --show-toplevel")))

(defun my-git-get-files (command)
  "Return the list of files from the specified Git COMMAND."
  (split-string (shell-command-to-string command) "\n" t))

(defun my-git-add-files (files)
  "Ask for confirmation before staging each file in FILES."
  (let ((total (length files)))
    (dolist (file files)
      (let ((index (1+ (cl-position file files :test 'equal))))
        (when (yes-or-no-p (format "Stage file %s? (%d/%d) " file index total))
          (shell-command (concat "git add " (shell-quote-argument file)))
          (message "Staging file: %s" file))
        ;; Fermer le minibuffer et donner le temps à Emacs de rafraîchir
        (message "")
        (sit-for 0.1))))
  (message "Staging complete."))

(defun my-git-run-command (command success-message failure-message)
  "Run a Git COMMAND and display a SUCCESS-MESSAGE if successful, or a FAILURE-MESSAGE if it fails."
  (with-temp-buffer
    (let ((exit-code (call-process-shell-command command nil t)))
      (if (eq exit-code 0)
          (message success-message)
        (let ((error-message (buffer-string)))
          (message "%s: %s" failure-message error-message))))))

(defun my-git-add-update ()
  "Run 'git add -u' from the root of the current Git repository. Ask for confirmation for each file before staging it."
  (interactive)
  (let ((default-directory (my-git-get-repo-root))
        (unstaged-files (my-git-get-files "git diff --name-only")))
    (my-git-add-files unstaged-files)))

(defun my-git-add-all ()
  "Run 'git add' from the root of the current Git repository. Ask for confirmation for each file before staging it."
  (interactive)
  (let* ((default-directory (my-git-get-repo-root))
         (unstaged-files (my-git-get-files "git diff --name-only"))
         (untracked-files (my-git-get-files "git ls-files --others --exclude-standard"))
         (all-files (append unstaged-files untracked-files)))
    (my-git-add-files(all-files))))

(defun my-git-commit ()
  "Run 'git commit' in the current directory. Prompt for a commit message in the minibuffer."
  (interactive)
  (let ((commit-message (read-string "Commit message: ")))
    (my-git-run-command (concat "git commit -m " (shell-quote-argument commit-message))
                        "Commit successful!" "Commit failed")))

(defun my-git-amend ()
  "Run 'git commit --amend --no-edit' in the current directory. If the commit fails, display the error message in the minibuffer."
  (interactive)
  (my-git-run-command "git commit --amend --no-edit"
                      "Amend successful!" "Amend failed"))

(defun my-git-push ()
  "Run 'git push origin' in the current directory. Prompt for the branch to push in the minibuffer. The default branch is the current branch."
  (interactive)
  (let* ((current-branch (string-trim (shell-command-to-string "git branch --show-current")))
         (branch (read-string (format "Branch to push (default %s): " current-branch) nil nil current-branch)))
    (my-git-run-command (concat "git push origin " (shell-quote-argument branch))
                        "Push successful!" "Push failed")))

(provide 'myfuns)
