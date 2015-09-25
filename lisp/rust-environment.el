;;; rust-environment --- Set up a pleasant rust development environment

;;; Commentary:
;;; Based on the article http://bassam.co/emacs/2015/08/24/rust-with-emacs/

;;; Code:

(require 'racer)
(setq racer-cmd "/usr/local/bin/racer")
;; The Rust source code for rust completions
(setq racer-rust-src-path "/Users/caleb/.rust/src/")

;; Rust-mode config
(add-hook
 'rust-mode-hook
 '(lambda ()
    (racer-mode)
    (eldoc-mode)
    (add-hook 'flyckec-mode-hook #'flycheck-rust-setup)
    (set (make-local-variable 'company-backends) '(company-racer))
    (local-set-key (kbd "M-.") #'racer-find-definition)
    (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
    (setq company-tooltip-align-annotations t)))

;; Make flycheck work correctly with rust, for instance, fixes extern crate
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide 'rust-environment)
;;; rust-environment.el ends here
