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
(global-set-key (kbd "C-c d") 'org-time-stamp-inactive)

(setq org-log-done 'time)

(setq org-directory "~/org/")

(setq org-default-notes-file "~/org/notes.org")
(setq org-deadline-warning-days 14)
(setq org-special-ctrl-a/e t)

(setq org-habit-show-habits-only-for-today nil)
(setq org-agenda-repeating-timestamp-show-all nil)

(require 'ox-latex)

(defun custom-org-setup ()
  "Setup org."
  (local-set-key (kbd "C-c t") #'org-show-todo-tree)
  (local-set-key (kbd "C-c (") #'reftex-citation))

(add-hook 'org-mode-hook 'reftex-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
;; (add-hook 'org-mode-hook 'flyspell-buffer)
(defun check-writing ()
  "Toggle `artbollocks-mode' and `writegood-mode' to check writing, then re-fontify the buffer."
  (interactive)
  (defvar artbollocks-mode nil)
  (defvar writegood-mode nil)
  
  (let ((status (not (and writegood-mode artbollocks-mode))))
    (artbollocks-mode status)
    (writegood-mode status)
    (font-lock-fontify-buffer)))
(global-set-key (kbd "C-c w") 'check-writing)

(add-to-list 'org-latex-classes
             '("myarticle"
               "\\documentclass[12pt]{article}

\\usepackage{geometry}
\\geometry{letterpaper, margin=0.95in}

\\usepackage[utf8]{inputenc}
\\usepackage{lmodern}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}
\\usepackage{setspace}
\\onehalfspacing

\\usepackage{titling}
\\pretitle{\\begin{center}\\singlespacing\\LARGE}
\\posttitle{\\par\\end{center}\\vspace{-1.2em}}
\\postauthor{\\end{tabular}\\par\\end{center}\\vspace{-1.5em}}
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

(add-to-list 'org-latex-classes
             '("paper"
               "\\documentclass[12pt]{article}

\\usepackage{geometry}
\\geometry{letterpaper, margin=1.0in}

\\usepackage[utf8]{inputenc}
\\usepackage{lmodern}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}
\\usepackage{setspace}
\\onehalfspacing

\\usepackage{titling}
\\pretitle{\\begin{center}\\singlespacing\\LARGE}
\\posttitle{\\par\\end{center}\\vspace{-1.2em}}
\\postauthor{\\end{tabular}\\par\\end{center}\\vspace{-1.5em}}
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

(defun is-agenda-buffer ()
  "Return non-nil if the current buffer is in the the org-agenda-files list."
  (member (abbreviate-file-name (buffer-file-name (current-buffer)))
          (mapcar #'abbreviate-file-name org-agenda-files)))

(defun close-org-files ()
  "Close all of the org agenda files."
  (interactive)
  (save-some-buffers nil #'is-agenda-buffer)
  (dolist (file org-agenda-files)
    (let ((buffer (buffer-with-file file)))
      (when buffer (kill-buffer buffer))))
  (message "closed all org agenda buffers"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (dot . t)))

(defun count-words-org-section (arg)
  "Count the number of words in an `org-mode' section.
Accepts a prefix argument ARG."
  (interactive "P")
  (if arg
      (message "%d words in top-level section" (count-words-top-level-section))
    (message "%d words in section"(count-words-section))))

(defun count-words-section ()
  "Count the words within a single `org-mode' section or subsection."
  (let ((position (point)))
    (outline-previous-visible-heading 1)
    (let ((start (point)))
      (outline-next-visible-heading 1)
      (let* ((end (point)) (count (count-words-region start end)))
        (goto-char position)
        count))))

(defun count-words-top-level-section ()
  "Count the words in a top level `org-mode' section."
  (let ((position (point)))
    (outline-up-heading 10000)
    (let ((start (point)))
      (org-forward-heading-same-level 1)
      (when (= start (point))
        (goto-char (point-max)))
      (let* ((end (point)) (count (count-words-region start end)))
        (goto-char position)
        count))))

(global-set-key (kbd "C-c v") 'count-words-org-section)

(provide 'org-config)
;;; org-config.el ends here
