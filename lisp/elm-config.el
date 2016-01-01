;;; elm-config --- configuration for the elm programming languages
;;; Commentary:

;;; Code:
(require 'company)
(require 'elm-mode)

(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
(add-to-list 'company-backends 'company-elm)

(provide 'elm-config)
;;; elm-config.el ends here
