;; org-config --- Configure org-mode
;;; Commentary:

;;; Code:
(require 'org)
(require 'org-inlinetask)
(require 'org-habit)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c r") 'org-capture)
(setq org-log-done 'time)

(setq org-directory "~/org/")

(setq org-default-notes-file "~/org/notes.org")
(setq org-deadline-warning-days 14)
(setq org-special-ctrl-a/e t)

(provide 'org-config)
;;; org-config.el ends here
