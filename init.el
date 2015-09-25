;;; init --- init

;;; Commentary:
;;; This is probably unnecessary in init.el, but flycheck wants it

;;; Code:
;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(setq-default indent-tabs-mode nil)
(setq latex-run-command "xelatex")
(setenv "PATH" (shell-command-to-string "source /etc/profile; source ~/.bash_profile; echo -n $PATH"))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
    version-control t)       ; use versioned backups

(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c C-f") 'find-file-at-point)

;; Load required packages
(load "~/.emacs.d/my-packages.el")
(load "~/.emacs.d/rust-environment.el")
(load "~/.emacs.d/company-config.el")
(load "~/.emacs.d/org-config.el")

(load-theme 'ample)
(electric-pair-mode)

(require 'ws-butler)
(ws-butler-global-mode +1)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; HASKELL STUFF

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(global-set-key (kbd "C-c ! !") 'flycheck-first-error)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(require 'merlin)
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

(defmacro compile-command-hook (hook command)
  "Set the compile command for mode HOOK to COMMAND."
  `(add-hook ,hook (lambda ()
                     (set (make-local-variable 'compile-command)
                          ,command))))
(compile-command-hook 'rust-mode-hook "cargo build")
(compile-command-hook 'haskell-mode-hook "cabal install")
(require 'egg)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "e8a9dfa28c7c3ae126152210e3ccc3707eedae55bdc4b6d3e1bb3a85dfb4e670" "ffe39e540469ef05808ab4b75055cc81266875fa4a0d9e89c2fec1da7a6354f3" "49eea2857afb24808915643b1b5bd093eefb35424c758f502e98a03d0d3df4b1" default)))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(haskell-literate-default (quote tex))
 '(org-agenda-files
   (quote
    ("~/org/homework.org" "~/org/hw/OntologicalPaper.org" "~/org/habits.org" "~/org/contacts.org" "~/org/ideas.org" "~/org/collegeapp.org" "~/org/todo.org" "~/org/notes.org")))
 '(org-capture-templates
   (quote
    (("n" "Notes" entry
      (file+headline "~/org/notes.org" "Notes")
      "")
     ("t" "Todo" entry
      (file+headline "~/org/todo.org" "Todo")
      "* TODO %?
  %U
  %a")
     ("h" "Homework" entry
      (file+headline "~/org/homework.org" "Homework")
      "* TODO %?
  %U
  %a")
     ("i" "Ideas" entry
      (file+headline "~/org/ideas.org" "Ideas")
      "* TODO %?
  %U
  %a"))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-inlinetask org-irc org-mhe org-rmail org-w3m)))
 '(org-refile-targets
   (quote
    ((nil :maxlevel . 1)
     (org-agenda-files :maxlevel . 1))))
 '(org-time-stamp-custom-formats (quote ("<%m/%d/%y %a>" . "<%m/%d/%y %a %H:%M>")))
 '(safe-local-variable-values
   (quote
    ((org-time-stamp-custom-formats "<%m %d>" . "<%Y-%m-%d>")
     (org-time-stamp-custom-formats "<%m %d>" . "<%Y-%m-%d %H:%M>")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(idris-prover-processed-face ((t (:background "color-22"))))
 '(idris-semantic-bound-face ((t (:inherit font-lock-constant-face))))
 '(idris-semantic-data-face ((t (:inherit font-lock-type-face))))
 '(idris-semantic-function-face ((t (:inherit font-lock-function-name-face))))
 '(idris-semantic-type-face ((t (:inherit font-lock-preprocessor-face))))
 '(idris-warning-face ((t (:inherit warning :underline t))))
 '(italic ((t (:underline t))))
 '(warning ((t (:foreground "color-214" :weight bold)))))

(provide 'init)
;;; init.el ends here
