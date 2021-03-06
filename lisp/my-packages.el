;; my-packages.el --- Summary

;;; Commentary:
;;; 

;;; Code:
(require 'package)

;; set up the package repos
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

;; list the requested packages
(defvar package-list
      '(ample-theme
        company
        flycheck
        flycheck-haskell
	merlin
	rust-mode
        flycheck-tip
        org
        writegood-mode
        artbollocks-mode
        elpy))

;; activate all the packages
(package-initialize)

;; fetch list of available packages
(unless package-archive-contents
  (package-refresh-contents))

;; install all the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (condition-case nil
        (package-install package)
      (error (read-from-minibuffer (format "Package '%s' failed to install. (Enter) " package))))))

(provide 'my-packages)
;;; my-packages.el ends here
