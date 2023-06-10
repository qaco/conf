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

(provide 'smarter-edition)
