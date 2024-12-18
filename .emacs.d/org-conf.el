(use-package org
  :ensure t)

(setq org-agenda-window-setup 'current-window)
(setq org-agenda-files (append (directory-files-recursively "~/org/agenda/" "\\.org$")
                               (directory-files-recursively "~/org/journal/" "\\.org$")))

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-enable-agenda-integration t
        org-journal-carryover-items nil
        org-journal-file-format "%Y-%m-%d.org"
        )
  )

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-todo-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

(provide 'org-conf)
