;; org-config --- Configure org-mode
;;; Commentary:

;;; Code:
(require 'org)
(when (< (string-to-number (org-version)) 8)
  (error "Org-mode is out of date (%s). Update by installing `org` from elpa in emacs -q" (org-version)))
(require 'org-inlinetask)
(require 'org-habit)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c r") 'org-capture)
(global-set-key (kbd "C-c s") 'close-org-files)

(setq org-log-done 'time)

(setq org-directory "~/org/")

(setq org-default-notes-file "~/org/notes.org")
(setq org-deadline-warning-days 14)
(setq org-special-ctrl-a/e t)

(setq org-habit-show-habits-only-for-today nil)
(setq org-agenda-repeating-timestamp-show-all nil)

(require 'ox-latex)

(add-to-list 'org-latex-classes
             '("myarticle"
               "\\documentclass[12pt]{article}

\\usepackage{geometry}
\\geometry{letterpaper, margin=0.75in}

\\usepackage[utf8]{inputenc}
\\usepackage{lmodern}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}
\\usepackage{setspace}
\\onehalfspacing

\\usepackage{fixltx2e}

               [NO-DEFAULT-PACKAGES]
               [NO-PACKAGES]
               [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("schoolpaper"
               "\\documentclass[12pt]{article}

\\usepackage{geometry}
\\geometry{letterpaper, margin=1in}

\\usepackage[utf8]{inputenc}
\\usepackage{lmodern}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}

\\usepackage{setspace}
\\doublespacing

\\usepackage{titling}
\\pretitle{\\begin{center}\\singlespacing\\LARGE}
\\posttitle{\\par\\end{center}\\vspace{-2em}}
\\postauthor{\\end{tabular}\\par\\end{center}\\vspace{-2em}}
\\setlength{\\droptitle}{-90pt}
\\postdate{\\par\\end{center}\\doublespacing\\vspace{-2.5em}}

\\usepackage{fixltx2e}
               [NO-DEFAULT-PACKAGES]
               [NO-PACKAGES]
               [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(global-set-key (kbd "C-c t") #'org-show-todo-tree)

(defun org-remove-headlines (backend)
  "Remove headlines with :notitle: tag."
  (org-map-entries (lambda () (let ((beg (point)))
                                (outline-next-visible-heading 1)
                                (backward-char)
                                (delete-region beg (point))))
                   "noexport" tree)
  (org-map-entries (lambda () (kill-whole-line)) "notitle"))

(add-hook 'org-export-before-processing-hook #'org-remove-headlines)

(defun filter (condp lst)
  "Remove any element for which CONDP produces nil.  Produces a copy of LST."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun buffer-with-file (file)
  "Return the buffer that corresponds to the FILE path."
  (catch 'break
    (dolist (buffer (buffer-list))
      (when (and (buffer-file-name buffer)
                 (equal (abbreviate-file-name file)
                        (abbreviate-file-name (buffer-file-name buffer))))
          (throw 'break buffer)))))
;; test code: (mapcar #'buffer-with-file org-agenda-files)

(defun close-org-files ()
  "Close all of the org agenda files."
  (interactive)
  (dolist (file org-agenda-files)
    (let ((buffer (buffer-with-file file)))
      (when buffer (kill-buffer buffer))))
  (message "closed all org agenda buffers"))

(provide 'org-config)
;;; org-config.el ends here
