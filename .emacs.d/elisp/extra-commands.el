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

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun newline-above ()
  (interactive)
  (end-of-line 0)
  (newline-and-indent))

(defun new-compile-cmd (nveau)
  "Change the compile command without compiling."
  (interactive "sNew compile command: ")
  (setq compile-command nveau))

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

(defun yank-and-indent ()
  (interactive)
  (yank)
  (indent-region (mark) (point)))

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

(defun llvm-cout ()
  (interactive)
  (insert "std::error_code err;\n")
  (insert "llvm::raw_fd_ostream stream(\"/dev/stdout\", err);\n"))

(provide 'extra-commands)
