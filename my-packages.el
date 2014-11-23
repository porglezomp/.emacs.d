;; my-packages.el
(require 'package)

;; set up the package repos
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

;; list the requested packages
(setq package-list
  '(
    ample-theme
    ))

;; activate all the packages
(package-initialize)

;; fetch list of available packages
(unless package-archive-contents
  (package-refresh-contents))

;; install all the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
